<template>
  <div class="card-panel" :class="props.alwaysShow ? 'alwaysShow' : ''" :style="{ cursor: props.cursor ? 'pointer' : '' }">
    <div v-if="props.icon" class="card-panel-icon-wrapper" :class="[`icon-${props.color}`]">
      <svg-icon :icon-class="props.icon" class-name="card-panel-icon" />
    </div>
    <div class="card-panel-description">
      <div class="card-panel-text" :style="{ color: props.textColor }">
        {{ props.name }}
      </div>
      <count-to
        :style="{ color: props.numColor }"
        :start-val="props.startVal"
        :end-val="props.endVal"
        :decimals="props.precision"
        :separator="props.separator"
        :duration="2600"
        class="card-panel-num"
      />
    </div>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

import { CountTo } from 'vue3-count-to'

const props = defineProps({
  name: {
    type: [String, Number],
    default: undefined
  },
  icon: {
    type: String,
    default: undefined
  },
  precision: {
    type: Number,
    default: 0
  },
  separator: {
    type: String,
    default: ','
  },
  startVal: {
    type: [Number, String],
    default: 0
  },
  endVal: {
    type: [Number, String],
    required: true
  },
  color: {
    type: String,
    default: 'blue'
  },
  textColor: {
    type: String,
    default: 'rgba(0, 0, 0, 0.45)'
  },
  numColor: {
    type: String,
    default: ''
  },
  cursor: {
    type: Boolean,
    default: false
  },
  alwaysShow: {
    type: Boolean,
    default: false
  }
})
</script>

<style lang="scss" scoped>
.card-panel {
  font-size: 12px;
  position: relative;
  overflow: hidden;
  color: #666;
  background: #fff;
  border: 1px solid #e6ebf5;
  box-shadow: 0 2px 12px 0 rgba(0, 0, 0, 0.1);
  border-radius: 4px;

  &:hover {
    .card-panel-icon-wrapper {
      color: #fff;
    }

    .icon-blueGreen {
      background: #40c9c6;
    }

    .icon-blue {
      background: #36a3f7;
    }

    .icon-red {
      background: #f4516c;
    }

    .icon-green {
      background: #34bfa3;
    }
  }

  .icon-blueGreen {
    color: #40c9c6;
  }

  .icon-blue {
    color: #36a3f7;
  }

  .icon-red {
    color: #f4516c;
  }

  .icon-green {
    color: #34bfa3;
  }

  .card-panel-icon-wrapper {
    float: left;
    margin: 14px 0 0 14px;
    padding: 16px;
    transition: all 0.38s ease-out;
    border-radius: 6px;
  }

  .card-panel-icon {
    float: left;
    font-size: 48px;
  }

  .card-panel-description {
    font-weight: bold;
    text-align: center;
    margin: 18px;

    .card-panel-text {
      line-height: 18px;
      color: rgba(0, 0, 0, 0.45);
      font-size: 16px;
      margin-bottom: 12px;
    }

    .card-panel-num {
      font-size: 40px;
      line-height: 40px;
    }
  }
}

.alwaysShow {
  .card-panel-icon-wrapper {
    color: #fff;
  }

  .icon-blueGreen {
    background: #40c9c6;
  }

  .icon-blue {
    background: #36a3f7;
  }

  .icon-red {
    background: #f4516c;
  }

  .icon-green {
    background: #34bfa3;
  }
}

@media (max-width: 550px) {
  .card-panel-icon-wrapper {
    display: none;
  }
}
</style>
