<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="date-item filter-item"
          style="width: 130px !important"
          placeholder="选择月"
          format="YYYY-MM"
          value-format="x"
          @change="crud.toQuery"
        />
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <common-radio-button
          v-model="query.taskTypeEnum"
          :options="queryTaskTypeENUM"
          type="enum"
          default
          class="filter-item"
          @change="crud.toQuery"
        />
        <common-radio-button
          type="enum"
          v-model="query.weightStatus"
          :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
          class="filter-item"
          @change="crud.toQuery"
        />
      </div>
    </template>
    <template #viewLeft>
      <slot name="viewLeft"></slot>
    </template>
  </crudOperation>
</template>

<script setup>
import { taskTypeENUM } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import moment from 'moment'

const defaultTime = moment().startOf('month').valueOf().toString()

const queryTaskTypeENUM = {
  ARTIFACT: taskTypeENUM.ARTIFACT,
  ASSEMBLE: { L: '部件', K: 'ASSEMBLE', V: taskTypeENUM.ASSEMBLE.V | taskTypeENUM.PARENT_PART.V },
  MACHINE_PART: taskTypeENUM.MACHINE_PART
}

const defaultQuery = {
  projectId: undefined,
  taskTypeEnum: undefined,
  dateTime: defaultTime.toString(),
  weightStatus: weightTypeEnum.NET.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
