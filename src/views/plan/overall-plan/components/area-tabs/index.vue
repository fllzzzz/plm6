<!-- 区域：tab选择 -->
<template>
  <el-tabs v-model="id" :tab-position="tabPosition" @tab-click="tabClick">
    <el-tab-pane v-for="item in areaInfo" :key="item.id" :label="`${item.name}(${item.axis})`" :name="`${item.id}`" />
  </el-tabs>
</template>

<script>

export default {
  props: {
    value: {
      type: [Number, String]
    },
    tabPosition: {
      type: String,
      default: 'top'
    },
    areaInfo: {
      type: Array
    },
    default: {
      type: Boolean,
      default: false
    },
    defaultTab: {
      type: Object
    }
  },
  data() {
    return {
      id: undefined,
      list: [],
      loading: false,
      oldValue: ''
    }
  },
  watch: {
    defaultTab: {
      handler(val) {
        this.id = val.name
        this.tabClick(this.defaultTab)
      },
      immediate: true,
      deep: true
    }
  },
  created() {
  },
  methods: {
    tabClick(val) {
      if (this.oldValue !== val.name) {
        this.oldValue = val.name
        this.$emit('update:value', val.name)
        this.$emit('tab-click', {
          name: val.name,
          label: val.label
        })
      }
    }
  }
}
</script>

<style lang="scss" scoped>
 .filter-item {
     /deep/.el-tabs__header{
        margin: 0;
        user-select:none;
        .el-tabs__item {
          line-height: 20px;
          height: 28px;
          padding: 0 14px;
          font-size: 13px;
        }
        .el-tabs__nav-next, .el-tabs__nav-prev {
          line-height: 20px;
        }
        .is-top,.is-bottom,.is-top,.is-bottom{
          :nth-child(2){
            padding-left: 0;
          }
          :last-child {
            padding-right: 0;
          }
        }
     }
 }
//  /deep/.el-tabs__header{
//          margin: 0;
//          .el-tabs__item {
//             line-height: 24px;
//             height: 30px;
//             padding: 0 14px;
//             font-size: 12px;
//         }
//      }

//  /deep/.el-tabs--top .el-tabs__item.is-top:nth-child(2),
//     .el-tabs--top .el-tabs__item.is-bottom:nth-child(2),
//     .el-tabs--bottom .el-tabs__item.is-top:nth-child(2),
//     .el-tabs--bottom .el-tabs__item.is-bottom:nth-child(2){
//         padding-left: 0;
//     }
//     .el-tabs--top .el-tabs__item.is-top:last-child,
//     .el-tabs--top .el-tabs__item.is-bottom:last-child,
//     .el-tabs--bottom .el-tabs__item.is-top:last-child,
//     .el-tabs--bottom .el-tabs__item.is-bottom:last-child{
//         padding-right: 0;
//     }
</style>
