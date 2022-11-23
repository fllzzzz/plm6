<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-if="crud.searchToggle">
          <project-cascader
            v-model="query.projectId"
            placeholder="所属项目"
            clearable
            class="filter-item"
            style="width: 250px"
            @change="crud.toQuery"
          />
          <el-date-picker
            v-model="query.endTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="生成日期"
            end-placeholder="生成日期"
            style="width: 240px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-input
            v-model.trim="query.cutTaskId"
            clearable
            style="width: 200px"
            size="small"
            placeholder="套料工单"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  endTime: [], // [开始日期，结束日期]
  projectId: undefined, // 项目id
  cutTaskId: undefined // 套料工单
}

const { crud, query } = regHeader(defaultQuery)

</script>
