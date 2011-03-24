var vows = require('vows'),
		assert = require('assert');

vows.describe('Array').addBatch({
    'An array': {
        'with 3 elements': {
            topic: [1, 2, 3],

            'has a length of 3': function (topic) {
                assert.equal(topic.length, 3);
            }
        },
        'with zero elements': {
            topic: [],

            'has a length of 0': function (topic) {
                assert.equal(topic.length, 0);
            },
            'returns *undefined*, when `pop()`ed': function (topic) {
                assert.isUndefined(topic.pop());
            }
        }
    }
}).run();