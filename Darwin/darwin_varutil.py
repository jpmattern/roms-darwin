#!/usr/bin/env python

import re
import logging
import numpy as np
from netCDF4 import Dataset

logger = logging.getLogger('darwin.varutil')

__version__ = '1.0.0'

__variables__ = {
    'palat': {'dtype': 'float', 'dimensions': ('nplank', 'nplank'), 'attributes': {'long_name': 'grazing palatability', 'units': 'dimensionless'}},
    'asseff': {'dtype': 'float', 'dimensions': ('nplank', 'nplank'), 'attributes': {'long_name': 'assimilation efficiency', 'units': 'dimensionless'}},
    'ExportFracPreyPred': {'dtype': 'float', 'dimensions': ('nplank', 'nplank'), 'attributes': {'long_name': 'grazing export fraction', 'units': 'dimensionless'}},
    'ExportFracMort': {'dtype': 'float', 'dimensions': ('nplank',), 'attributes': {'long_name': 'fraction of linear mortality to POM', 'units': 'dimensionless'}},
    'ExportFracMort2': {'dtype': 'float', 'dimensions': ('nplank',), 'attributes': {'long_name': 'fraction of quadratic mortality to POM', 'units': 'dimensionless'}},
    'grazemax': {'dtype': 'float', 'dimensions': ('nplank',), 'attributes': {'long_name': 'plankton maximum grazing rate', 'units': 's-1'}},
    'mort2': {'dtype': 'float', 'dimensions': ('nplank',), 'attributes': {'long_name': 'quadratic mortality coefficient', 'units': '(mmol C m-3)-1 s-1'}},
}

__default_values__ = {
    (6, 4): {
        'palat': np.array([[0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [1, 1, 0.27, 0.3, 0.27, 0, 0, 0, 0, 0],
                           [1, 1, 0.27, 0.3, 0.27, 0, 0, 0, 0, 0],
                           [0.3, 0.3, 0.9, 1, 0.9, 0.3, 0.3, 0, 0, 0],
                           [0, 0, 0.27, 0.3, 0.27, 1, 1, 0, 0, 0],
                           [0, 0, 0, 0, 0, 0.3, 0.3, 1, 1, 0]]),
        'asseff': np.array([[0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.7, 0.7, 0.5, 0.5, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.7, 0.7, 0.5, 0.5, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1],
                            [0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.7, 0.7, 0.7, 0.7, 0.7, 0.1, 0.1, 0.1],
                            [0.1, 0.1, 0.1, 0.1, 0.1, 0.7, 0.7, 0.7, 0.7, 0.1]]),
        'ExportFracPreyPred': np.full((10, 10), fill_value=0.4),
        'ExportFracMort': np.array([0.1, 0.1375, 0.175, 0.175, 0.175, 0.2125, 0.2, 0.275, 0.3125, 0.35]),
        'ExportFracMort2': np.array([0.1, 0.1375, 0.175, 0.175, 0.175, 0.2125, 0.2, 0.275, 0.3125, 0.35]),
        'grazemax': np.array([0, 0, 0, 0, 0, 1.28094227939622e-05, 0.000113618845587924, 0.000113618845587924, 5.33495023166272e-05, 1.65569851528889e-05]),
        'mort2': np.array([9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 9.25925925925926e-06, 5e-05]),
    }
}


def convert_darwin_config(config):
    if isinstance(config, tuple):
        return config
    else:
        m = re.match('(.*[^0-9])?(?P<num_p>[0-9]+)[Pp](?P<num_z>[0-9]+)[Zz]((?P<num_b>[0-9]+)[Bb])?', config)
        if not m:
            raise ValueError(f'Invalid config "{config}".')
        gd = m.groupdict()
        if 'num_b' in gd and gd['num_b'] is not None:
            return (int(gd['num_p']), int(gd['num_z']), int(gd['num_b']))
        return (int(gd['num_p']), int(gd['num_z']))


def detect_darwin_config(ncfile=None, nc=None, return_str=False):
    if nc is None:
        logger.debug(f'opening "{ncfile}" to detect Darwin configuration')
        with Dataset(ncfile) as nc:
            return detect_darwin_config(nc=nc)

    if 'phytoplankton01' in nc.variables:
        logger.debug('going through variables')

        n_p = 1
        while f'phytoplankton{n_p:02d}' in nc.variables:
            n_p += 1
        n_p -= 1

        n_z = 1
        while f'zooplankton{n_z:02d}' in nc.variables:
            n_z += 1
        n_z -= 1

        n_b = 1
        while f'bacteria{n_b:02d}' in nc.variables:
            n_b += 1
        n_b -= 1
    elif 'NLM_LBC' in nc.ncattrs():
        logger.debug('could not find plankton variables, going through "NLM_LBC" attribute (linear boundary conditions attribute)')

        att = nc.getncattr('NLM_LBC')

        n_p = 1
        while f'phytoplankton{n_p:02d}' in att:
            n_p += 1
        n_p -= 1

        n_z = 1
        while f'zooplankton{n_z:02d}' in att:
            n_z += 1
        n_z -= 1

        n_b = 1
        while f'bacteria{n_b:02d}' in att:
            n_b += 1
        n_b -= 1
    else:
        raise RuntimeError('Could not find Darwin plankton information.')

    logger.debug(f'found {n_p} phytoplankton, {n_z} zooplankton, and {n_b} bacteria')

    if return_str:
        if n_b > 0:
            return f'{n_p}P{n_z}Z{n_b}B'
        return f'{n_p}P{n_z}Z'
    if n_b > 0:
        return (n_p, n_z, n_b)
    return (n_p, n_z)


def add_variable(ncfile, variable, values=None, darwinconfiguration=None, nplank=None, darwindatafile=None):
    if 'long_name' in __variables__[variable]['attributes']:
        logger.info(f'adding variable "{variable}" ({__variables__[variable]["attributes"]["long_name"]})')
    else:
        logger.info(f'adding variable "{variable}"')

    if darwinconfiguration is None:
        logger.info('Darwin configuration not specified, attempting to detect configuration from file')
        darwinconfiguration = detect_darwin_config(ncfile)
        if np.sum(darwinconfiguration) == 0:
            raise RuntimeError('Could not detect Darwin configuration from file, Darwin configuration must be specified.')

    elif not isinstance(darwinconfiguration, tuple):
        darwinconfiguration = convert_darwin_config(darwinconfiguration)

    nplank = np.sum(darwinconfiguration)
    logger.debug(f'setting nplank={nplank}')

    dimensions = {
        'nplank': nplank,
    }

    if values is not None:
        logger.debug(f'using user-specified {variable} values')
    elif darwindatafile is not None:
        logger.info(f'reading {variable} values from "{darwindatafile}".')
        with open(darwindatafile) as f:
            read = False
            for line in f:
                if re.match('^ *'+variable+' *= *\n', line, flags=re.IGNORECASE):
                    read = True
                    values = []
                elif re.match('^ *'+variable+' *= *[0-9]+ *\*.*, *\n', line, flags=re.IGNORECASE):
                    # something like:
                    # ASSEFF=  100*0.1 ,
                    m = re.match('^ *'+variable+' *= *[0-9]+ *\* *([0-9.e+-]+) *, *\n', line, flags=re.IGNORECASE)
                    values = np.empty(shape=[dimensions[d] for d in __variables__[variable]['dimensions']])
                    values[:] = float(m.group(1))
                    read = True
                elif read:
                    if re.match('^ *[0-9eE.+-]+,', line.strip()):
                        values.extend([float(v) for v in line.strip().strip(',').split()])
                    else:
                        m = re.match('^ *'+variable+'\( *([0-9]+) *, *([0-9]+) *\) *= *([^ ,!]+).*', line, flags=re.IGNORECASE)
                        if not m:
                            break
                        # ROMS wants it backwards
                        values[int(m.group(2))-1, int(m.group(1))-1] = float(m.group(3))
            values = np.asarray(values)
    else:
        logger.debug(f'attempting to use default {variable} values')
        if darwinconfiguration not in __default_values__:
            raise KeyError(f'No default values available for {darwinconfiguration} Darwin configuration.')
        if variable not in __default_values__[darwinconfiguration]:
            raise KeyError(f'No default values available for "{variable}" in {darwinconfiguration} Darwin configuration.')
        values = __default_values__[darwinconfiguration][variable]

    with Dataset(ncfile, 'a') as nc:
        if variable in nc.variables:
            logger.info(f'file "{ncfile}" already contains variable "{variable}"')
        else:
            for d in __variables__[variable]['dimensions']:
                if d not in nc.dimensions:
                    if dimensions[d] is None:
                        raise ValueError(f'The size for dimension "{d}" needs to be specified.')
                    nc.createDimension(d, dimensions[d])
                elif dimensions[d] != nc.dimensions[d].size:
                    raise ValueError(f'Size for dimension "{d}" in file ({nc.dimensions[d].size}) does not match required size set by the Darwin configuration ({dimensions[d]}).')
            var = nc.createVariable(variable, __variables__[variable]['dtype'], dimensions=__variables__[variable]['dimensions'])
            var.setncatts(__variables__[variable]['attributes'])

        if values is None:
            nc.variables[variable][:] = 0.0
        else:
            logger.info(f'writing {variable} values to "{ncfile}"')
            try:
                if len(values) == 1:
                    nc.variables[variable][:] = values[0]
                else:
                    nc.variables[variable][:] = values
            except ValueError:
                raise ValueError(f'Must specify {nc.variables[variable].size} values for variable "{variable}".')


def add_biosedvariables(ncfile):
    with Dataset(ncfile, 'a') as nc:
        for v in ('PIC_sed','POC_sed','PON_sed','POSi_sed','POFe_sed','POP_sed','plankton_C_sed','chl_sed'):
            if v in nc.variables:
                logger.info(f'file "{ncfile}" already contains variable "{v}"; it will not be modified')
                continue
            logger.info(f'adding variable "{v}"')
            nc.createVariable(v,'float',dimensions=('ocean_time', 'eta_rho', 'xi_rho'))
            nc.variables[v][:] = 0.0


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Perform a Darwin file utility function.')

    subparsers = parser.add_subparsers(title='utility function', dest='type', required=True)

    # subparsers
    parsers = {}

    variable_choices = list(__variables__.keys())
    variable_choices.append('all')

    parsers['add-var'] = subparsers.add_parser('add-var', help='Add a Darwin variable to a ROMS (initial) NetCDF file.')
    parsers['add-var'].add_argument('ncfile', type=str, help='A ROMS netcdf file.')
    parsers['add-var'].add_argument('variable', type=str, choices=variable_choices, help='The variable to add.')
    parsers['add-var'].add_argument('--darwinconfiguration', type=str, default=None, help='The Darwin configuration string (for example "6P4Z"; default: attempt to read from NCFILE).')
    mut = parsers['add-var'].add_mutually_exclusive_group(required=True)
    mut.add_argument('--values', type=float, default=None, nargs='+', help='The values to set the variable to.')
    mut.add_argument('--fromfile', type=str, default=None, help='The MITgcm Darwin data file to read the values for the variable from.')
    mut.add_argument('--defaultvalues', action='store_true', help='Use default values for the variable.')

    parsers['add-biosed'] = subparsers.add_parser('add-biosed', help='Add Darwin variables for biological sediment to a ROMS (initial) NetCDF file.')
    parsers['add-biosed'].add_argument('ncfile', type=str, help='A ROMS netcdf file.')

    for p in parsers:
        parsers[p].add_argument('--debug', action='store_true', help='Show debug output.')

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)
    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
        for name in ('matplotlib', 'PIL'):
            logging.getLogger(name).setLevel(logging.WARNING)

    if args.type == 'add-var':
        if args.variable == 'all':
            if args.values is not None:
                parser.error(f'values option cannot be combined with variable choice: "{args.variable}" ')
            for variable in __variables__.keys():
                add_variable(args.ncfile, variable, darwinconfiguration=args.darwinconfiguration, values=args.values, darwindatafile=args.fromfile)
        else:
            add_variable(args.ncfile, args.variable, darwinconfiguration=args.darwinconfiguration, values=args.values, darwindatafile=args.fromfile)
    elif args.type == 'add-biosed':
        add_biosedvariables(args.ncfile)

