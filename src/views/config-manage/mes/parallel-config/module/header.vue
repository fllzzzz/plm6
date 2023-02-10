<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <common-select
        v-model="query.thick"
        :options="thick"
        type="other"
        clearable
        :data-structure="{ key: 'value', label: 'value', value: 'value' }"
        class="filter-item"
        style="width: 200px"
        placeholder="请选择厚度"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.material"
        :options="material"
        type="other"
        clearable
        :data-structure="{ key: 'value', label: 'value', value: 'value' }"
        class="filter-item"
        style="width: 200px"
        placeholder="请选择材质"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { getParallelParams } from '@/api/config/system-config/parallel-config'

import { regHeader } from '@compos/use-crud'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  serialNumber: undefined,
  specification: undefined
}

const { crud, query } = regHeader(defaultQuery)
const material = ref([])
const thick = ref([])

getParams()

async function getParams() {
  material.value = []
  thick.value = []
  try {
    const data = await getParallelParams()
    for (let i = 0; i < data.material.length; i++) {
      material.value.push({ value: data.material[i] })
    }
    for (let i = 0; i < data.thick.length; i++) {
      thick.value.push({ value: data.thick[i] })
    }
  } catch (e) {
    console.log('获取厚度材质筛选', e)
  }
}
</script>
