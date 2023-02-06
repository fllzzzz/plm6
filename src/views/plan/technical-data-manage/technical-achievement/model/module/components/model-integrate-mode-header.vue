<template>
  <crudOperation>
    <template #optLeft>
      <common-button
        v-permission="crud.permission.integration"
        :loading="integrationLoading"
        type="primary"
        class="filter-item"
        size="mini"
        @click="toIntegrationModel"
        :disabled="!areaIds?.length || hasProcessingIM"
        >集成模型</common-button
      >
      <common-button
        v-permission="crud.permission.integrationDel"
        type="danger"
        size="mini"
        class="filter-item"
        :loading="resetLoading"
        @click="toDelIntegrationModel"
        v-if="hasIntegrationModel"
        :disabled="hasProcessingIM"
        >重置集成模型</common-button
      >
    </template>
    <template #viewLeft>
      <slot name="viewLeft"></slot>
    </template>
  </crudOperation>
</template>

<script setup>
import { inject, defineProps, ref } from 'vue'
import { integrationModel, integrationModelDel } from '@/api/bim/model'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { ElNotification, ElMessageBox } from 'element-plus'

const props = defineProps({
  info: {
    type: Object
  }
})

const defaultQuery = {}

const { crud } = regHeader(defaultQuery)

const areaIds = inject('areaIds')
const hasIntegrationModel = inject('hasIntegrationModel')
const hasProcessingIM = inject('hasProcessingIM')
const integrationLoading = ref(false)
const resetLoading = ref(false)

// 集成模型
async function toIntegrationModel() {
  try {
    if (!areaIds.value?.length) return
    integrationLoading.value = true
    await integrationModel({ areaIds: areaIds.value, monomerId: props.info.id })
    ElNotification({ title: '集成模型请求发送成功', type: 'success' })
    crud.refresh()
  } catch (error) {
    console.log('集成模型失败', error)
  } finally {
    integrationLoading.value = false
  }
}

// 重置集成模型
async function toDelIntegrationModel() {
  ElMessageBox.confirm(`是否确认重置 “${props.info.name}” 单体下的集成模型`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      resetLoading.value = true
      await integrationModelDel({ monomerId: props.info.id })
      ElNotification({ title: '重置集成模型成功', type: 'success' })
      crud.refresh()
    } catch (error) {
      console.log('重置集成模型失败', error)
    } finally {
      resetLoading.value = false
    }
  })
}
</script>
