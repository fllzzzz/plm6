<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.areaId"
        :options="query.monomerId && monomerMap?.[query.monomerId]?.children || []"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        clearable
        type="other"
        placeholder="可选择区域"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="编号搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag v-if="props.tip" class="filter-item" size="medium" effect="plain">{{ props.tip }}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import useProjectTree from '@compos/store/use-project-tree'

const props = defineProps({
  tip: {
    type: String,
    default: '覆盖导入仅支持PDF格式'
  }
})

const defaultQuery = {
  areaId: undefined,
  serialNumber: undefined
}

const { CRUD, crud, query } = regHeader(defaultQuery)
const { monomerMap } = useProjectTree()

// 阻断重置，初始化参数
CRUD.HOOK.beforeResetQuery = () => {
  query.areaId = undefined
  query.serialNumber = undefined
  crud.toQuery()
  return false
}

</script>
