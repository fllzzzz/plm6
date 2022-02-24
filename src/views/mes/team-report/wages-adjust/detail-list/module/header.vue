<template>
  <div style="display: flex; justify-content: space-between">
    <div v-show="crud.searchToggle">
      <el-tag class="filter-item" size="medium">
        {{ tipText }}
      </el-tag>
      <el-input
        v-model.trim="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <div>
      <slot name="auditBox"></slot>
    </div>
  </div>
</template>

<script setup>
import { computed, defineProps } from 'vue'

import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const props = defineProps({
  fInfo: {
    type: Object,
    default: () => {}
  }
})

const defaultQuery = {}

const tipText = computed(() => {
  const { name, steelSpec, material, category } = props.fInfo || {}
  console.log(name, steelSpec, material, category)
  if (category) {
    return mesEnclosureTypeEnum.VL[category]
  } else if (name) {
    return name + `${steelSpec ? '-' + steelSpec : ''}` + `${material ? '-' + material : ''}`
  } else {
    return '未选择'
  }
})

const { crud, query } = regHeader(defaultQuery)
</script>
