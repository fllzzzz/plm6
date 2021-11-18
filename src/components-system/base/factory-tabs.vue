<!-- 工厂：tab选择 -->
<template>
  <el-skeleton :loading="!loaded" :rows="1" animated>
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
import { defineEmits, defineProps, ref } from 'vue'
import { isNotBlank } from '@data-type/index'

import useFactory from '@compos/store/use-factories'
import { ElTabs, ElTabPane } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'tab-click'])

const props = defineProps({
  modelValue: {
    type: Number
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
let factoriesMap = new Map()

const { loaded, factories } = useFactory(dataFormat)

function tabClick() {
  const factory = factoriesMap.get(factoryId.value)
  emit('update:modelValue', factory.id)
  emit('tab-click', {
    id: factory.id,
    name: factory.name
  })
}

function dataFormat() {
  factoriesMap = new Map()
  try {
    if (isNotBlank(factories.value)) {
      // key 使用字符串，因为tab的v-model是字符串类型
      factories.value.forEach(v => {
        factoriesMap.set(`${v.id}`, v)
      })
      factoryId.value = `${factories.value[0].id}`

      tabClick({
        id: factoryId.value,
        name: `${factories.value[0].name}`
      })
    }
  } catch (error) {
    console.log('获取工厂', error)
  }
}
</script>
