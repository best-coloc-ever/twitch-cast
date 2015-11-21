import os
import json

from flask import request, jsonify
from functools import wraps
from jsonschema import Draft4Validator

SCHEMA_DIR = '/schemas'
SCHEMA_EXTENSION = '.schema.json'

def validate_json_request(schema_name):
    schema_file_name = '{}{}'.format(schema_name, SCHEMA_EXTENSION)
    schema_file_path = os.path.join(SCHEMA_DIR, schema_file_name)

    with open(schema_file_path, 'r') as f:
        schema = json.load(f)

    validator = Draft4Validator(schema)

    def decorate(fn):
        @wraps(fn)

        def wrapped(*args, **kwargs):
            payload = request.get_json()

            if payload is None:
                return 'Expected JSON', 400

            errors = [
                error.message
                for error in validator.iter_errors(payload)
            ]

            if len(errors):
                return jsonify(errors=errors), 400

            return fn(payload, *args, **kwargs)

        return wrapped

    return decorate

def preprocess(action):

    def decorate(fn):
        @wraps(fn)

        def wrapped(*args, **kwargs):
            action()
            return fn(*args, **kwargs)

        return wrapped

    return decorate
