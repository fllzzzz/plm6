<!-- 工厂：tab选择 -->
<template>
  <el-skeleton :loading="loading" :rows="1" animated>
    <template #template>
      <el-skeleton-item variant="rect" style="margin-top: 10px; height: 30px; margin-bottom: 10px" />
    </template>
    <template #default>
      <el-tabs v-if="isNotBlank(factories)" v-model="factoryId" :tab-position="props.tabPosition" :type="props.type" @tab-click="tabClick">
        <el-tab-pane v-for="item in factories" :key="item.id" :label="`${item.name}`" :name="`${item.id}`" />
        <slot name="content" />
      </el-tabs>
      <el-tag v-else type="danger" style="margin:15px 0">* 暂无工厂, 请添加工厂后再添加仓库</el-tag>
    </template>
  </el-skeleton>
</template>

<script setup>
import { getFactoriesAllSimple as getAll } from '@/api/mes/common'
import { defineEmits, defineProps, ref } from 'vue'
import { isNotBlank } from '@data-type/index'
import { ElTabs, ElTabPane } from 'element-plus'

const emit = defineEmits(['update:moduleValue', 'tab-click'])

const props = defineProps({
  moduleValue: {
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
})

const factoryId = ref()
const factories = ref([])
const loading = ref(false)

fetchFactories()

function tabClick(val) {
  emit('update:moduleValue', val.name)
  emit('tab-click', {
    name: val.name,
    label: val.label
  })
}

async function fetchFactories() {
  loading.value = true
  let _factories = []
  try {
    const { content = [] } = await getAll()
    _factories = content
    if (isNotBlank(factories)) {
      factoryId.value = `${_factories[0].id}`
      tabClick({
        name: factoryId.value,
        label: `${_factories[0].name}`
      })
    }
  } catch (error) {
    console.log('获取工厂', error)
  } finally {
    factories.value = _factories
    loading.value = false
  }
}
</script>
