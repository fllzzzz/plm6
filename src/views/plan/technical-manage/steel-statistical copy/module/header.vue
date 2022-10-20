<template>
  <div>
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            class="date-item filter-item"
            style="width:100px!important"
            placeholder="选择年"
            format="YYYY"
            value-format="YYYY"
            @change="crud.toQuery"
          />
        </template>
      </crudOperation>
      <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel name="项目领料总量" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="DP.COM_WT__KG" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="构件总重量" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.processQuantity || 0" :precision="DP.COM_WT__KG" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="损耗" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.jobQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="损耗率（%）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.processingQuantity || 0" :precision="0" />
        </el-col>
      </el-row>
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { regHeader } from '@compos/use-crud'

import { DP } from '@/settings/config'

import crudOperation from '@crud/CRUD.operation'
import Panel from '@/components/Panel'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const defaultQuery = {
  year: undefined,
  projectId: undefined
}

const { crud, query } = regHeader(defaultQuery)

const totalAmount = ref({})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)
</script>
