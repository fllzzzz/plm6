<!-- 工厂：tab选择 -->
<template>
  <el-tabs
    v-model="factoryId"
    :tab-position="tabPosition"
    :type="type"
    @tab-click="tabClick"
  >
    <el-tab-pane v-for="item in factories" :key="item.id" :label="`${item.name}`" :name="`${item.id}`" />
    <slot name="content" />
  </el-tabs>
</template>

<script>
import { getFactoriesAllSimple } from '@/api/common'
// import checkPermission from '@/utils/permission'

// const permission = {
//   get: ['factory:getAllSimple']
// }

export default {
  props: {
    // eslint-disable-next-line vue/require-default-prop
    value: {
      type: String
    },
    tabPosition: {
      type: String,
      default: 'top'
    },
    type: {
      type: String,
      default: ''
    }
  },
  data() {
    return {
      // permission,
      factoryId: undefined,
      factories: []
    }
  },
  created() {
    this.fetchFactories()
  },
  methods: {
    tabClick(val) {
      this.$emit('update:value', val.name)
      this.$emit('tab-click', {
        name: val.name,
        label: val.label
      })
    },
    async fetchFactories() {
      // if (!checkPermission(permission.get)) {
      //   return
      // }
      let factories = []
      try {
        const res = await getFactoriesAllSimple()
        if (res && res.content && res.content.length > 0) {
          factories = res.content
          this.factoryId = `${factories[0].id}`
          this.tabClick({
            name: this.factoryId,
            label: `${factories[0].name}`
          })
        }
      } catch (error) {
        console.log('获取工厂', error)
      } finally {
        this.factories = factories
      }
    }
  }
}
</script>
