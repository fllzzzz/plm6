<template>
  <div v-show="crud.searchToggle">
    <slot name="customSearch" />
    <rrOperation />
  </div>
  <crudOperation>
    <template v-slot:viewLeft>
      <el-tag v-permission="permission.get" effect="plain" class="filter-item" size="medium">
        累计生产量：
        <slot v-if="!summaryLoading" name="summaryText" :summary="summaryData"></slot>
        <i v-else class="el-icon-loading" />
      </el-tag>
    </template>
  </crudOperation>
</template>

<script setup>
import { inject, ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = inject('defaultQuery')

const { crud, query, CRUD } = regHeader(defaultQuery)

const permission = inject('permission')

CRUD.HOOK.afterToQuery = () => {
  fetchSummary()
}

const summaryData = ref({})
const summaryLoading = ref(false)
const getSummaryApi = inject('getSummaryApi')

async function fetchSummary() {
  if (!checkPermission(permission.get)) {
    return
  }
  try {
    summaryLoading.value = true
    summaryData.value = await getSummaryApi(query)
  } catch (error) {
    console.log('获取汇总信息', error)
  } finally {
    summaryLoading.value = false
  }
}
</script>
