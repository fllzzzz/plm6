<template>
  <el-table-column v-if="projectOptions" prop="projectId" align="center" min-width="170px" label="所属项目">
    <template #default="{ row }">
      <common-select
        v-model="row.projectId"
        :options="projectOptions"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        :extra-val="dittos.get('type')"
        show-extra
        type="other"
        placeholder="所属项目"
      />
    </template>
  </el-table-column>
  <el-table-column prop="factoryId" align="center" min-width="120px" label="工厂">
    <template #default="{ row, $index }">
      <factory-select
        v-model="row.factoryId"
        placeholder="请选择工厂"
        only-one-default
        :extra-val="dittos.get('type')"
        show-extra
        @change="handleFactoryChange($event, $index)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="warehouseId" label="存储位置" min-width="140px" align="center">
    <template #default="{ row, $index }">
      <warehouse-select
        v-model="row.warehouseId"
        :factory-id="getFactoryVal($index)"
        :basicClass="row.basicClass"
        :extra-val="dittos.get('type')"
        show-extra
        placeholder="存储位置"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { computed, watch, inject } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import { projectNameFormatter } from '@/utils/project'

import { regExtra } from '@/composables/form/use-form'
import factorySelect from '@/components-system/base/factory-select.vue'
import warehouseSelect from '@/components-system/wms/warehouse-select.vue'
import useDittoRealVal from '@/composables/form/use-ditto-real-val'

const { cu, form } = regExtra() // 表单
const dittos = inject('dittos')
const {
  initScopeList: initFactoryScopeList,
  handleValueChange: handleFactoryChange,
  getRealVal: getFactoryVal
} = useDittoRealVal('factoryId', dittos.get('factoryId'))

// 申购单选择
const projectOptions = computed(() => {
  const order = cu.props.order
  if (isNotBlank(order) && isNotBlank(order.projects)) {
    return order.projects.map((p) => {
      return { id: p.id, name: projectNameFormatter(p, { showSerialNumber: false }) }
    })
  } else {
    return null
  }
})

// 如果只有一个项目自动赋值
watch(
  projectOptions,
  (opts) => {
    if (opts && opts.length === 1 && form.list[0]) form.list[0].projectId = opts[0].id
  },
  { immediate: true }
)

watch(
  () => form.list,
  () => {
    initFactoryScopeList(form.list || [])
  },
  { immediate: true }
)
</script>
