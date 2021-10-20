<template>
  <div class="card-panel" :class="alwaysShow?'alwaysShow':''" :style="{'cursor': cursor ? 'pointer':''}">
    <div v-if="icon" class="card-panel-icon-wrapper" :class="[`icon-${color}`]">
      <svg-icon :icon-class="icon" class-name="card-panel-icon" />
    </div>
    <div class="card-panel-description">
      <div class="card-panel-text">
        {{ name }}
      </div>
      <count-to :style="{'color':numColor}" :start-val="startVal" :end-val="endVal" :decimals="decimals" :separator="separator" :duration="2600" class="card-panel-num" />
    </div>
  </div>
</template>

<script>
import CountTo from 'vue-count-to'

export default {
  components: {
    CountTo
  },
  props: {
    // eslint-disable-next-line vue/require-default-prop
    name: {
      type: [String, Number]
    },
    icon: {
      type: String,
      default: undefined
    },
    decimals: {
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
  }
}
</script>

<style lang="scss" scoped>
  .card-panel {
    height: 108px;
    font-size: 12px;
    position: relative;
    overflow: hidden;
    color: #666;
    background: #fff;
    border: 1px solid #e6ebf5;
    box-shadow: 0 2px 12px 0 rgba(0,0,0,0.1);
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
        background: #34bfa3
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
      color: #34bfa3
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
      float: right;
      font-weight: bold;
      margin: 26px;
      margin-left: 0px;
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      align-items: flex-end;

      .card-panel-text {
        line-height: 18px;
        color: rgba(0, 0, 0, 0.45);
        font-size: 16px;
        margin-bottom: 12px;
      }

      .card-panel-num {
        font-size: 20px;
      }
    }
 }

  .alwaysShow{
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
          background: #34bfa3
        }
  }

@media (max-width:550px) {
  .card-panel-icon-wrapper {
    display: none;
  }

//   .card-panel-icon-wrapper {
//     float: none !important;
//     width: 100%;
//     height: 100%;
//     margin: 0 !important;

//     .svg-icon {
//       display: block;
//       margin: 14px auto !important;
//       float: none !important;
//     }
//   }
}
</style>
