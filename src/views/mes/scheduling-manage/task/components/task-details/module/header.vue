<template>
  <div v-show="crud.searchToggle">
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 250px" @change="crud.toQuery" />
    <production-line-select
      v-model="query.productionLineId"
      :factoryId="query.factoryId"
      clearable
      class="filter-item"
      style="width: 250px"
      @change="crud.toQuery"
    />
    <monomer-select
      v-model="query.monomerId"
      :default="false"
      clearable
      :project-id="query.projectId"
      class="filter-item"
      style="width: 250px"
      @getAreaInfo="getAreaInfo"
      @change="crud.toQuery"
    />
    <common-select
      v-model="query.areaId"
      :options="areaList"
      size="small"
      :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
      clearable
      :noDataText="query.monomerId ? '暂无数据' : '未选择单体'"
      class="filter-item"
      placeholder="请选择区域"
      style="width: 250px"
      @change="crud.toQuery"
    />
  </div>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.issueStatus"
      :options="taskIssueTypeEnum.ENUM"
      showOptionAll
      size="small"
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <product-type-query :productType="productType" :category="query.category" :toQuery="crud.toQuery" :query="query" />
    <rrOperation />
  </div>
  <crudOperation>
    <template v-slot:optLeft>
      <template v-if="query.issueStatus !== taskIssueTypeEnum.HAS_ISSUED.V">
        <template v-if="modifying">
          <el-tag type="info" style="margin-right: 5px" size="medium">当前操作：{{ operateButtonEnumV[buttonValue].L }}</el-tag>
          <common-button type="success" size="mini" @click="previewIt">预览并保存</common-button>
          <common-button type="warning" size="mini" @click.stop="handelModifying(false, true)">取消</common-button>
        </template>
        <template v-else>
          <common-button
            v-for="item in operateButtonEnum"
            :key="item.V"
            style="margin-right: 5px"
            :type="item.T"
            size="mini"
            @click.stop="operateIt(item.V)"
          >
            {{ item.L }}
          </common-button>
        </template>
      </template>
    </template>
    <template v-slot:viewLeft>
      <template v-if="query.issueStatus !== taskIssueTypeEnum.HAS_ISSUED.V">
        <template v-if="modifying">
          <el-tag type="info" style="margin-right: 5px" size="medium">快捷操作</el-tag>
          <el-date-picker
            style="margin-right: 5px"
            v-model="askCompleteTime"
            type="date"
            size="mini"
            :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
            placeholder="需求完成日期"
          />
          <common-button type="success" size="mini" @click.stop="applyAll">全部应用</common-button>
        </template>
      </template>
    </template>
  </crudOperation>
</template>

<script setup>
import { defineProps, defineEmits, defineExpose, ref, inject } from 'vue'
import { ElMessage } from 'element-plus'
import moment from 'moment'

import { processTypeEnum, taskIssueTypeEnum } from '@enum-ms/mes'
import EO from '@/utils/enum'

import { regHeader } from '@compos/use-crud'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import FactorySelect from '@/components-system/base/factory-select.vue'
import productionLineSelect from '@comp-mes/production-line-select'
import monomerSelect from '@/components-system/plan/monomer-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const operateButtonEnum = {
  TASK_ISSUE: { L: '任务下发', K: 'TASK_ISSUE', V: 1, T: 'primary' }
  // MODIFY_TASK: { L: '修改排产任务', K: 'MODIFY_TASK', V: 2, T: 'warning' }
}
const operateButtonEnumV = EO.key2val(operateButtonEnum)

const emit = defineEmits(['update:modifying', 'preview'])
defineProps({
  modifying: {
    type: Boolean,
    default: false
  }
})

const defaultQuery = {
  processType: processTypeEnum.ONCE.V,
  issueStatus: taskIssueTypeEnum.NOT_ISSUED.V,
  serialNumber: undefined
}
const { crud, query } = regHeader(defaultQuery)

const productType = inject('productType')
const buttonValue = ref()
const askCompleteTime = ref()

const areaList = ref([])
function getAreaInfo(list) {
  areaList.value = list
}

// 操作
function operateIt(v) {
  buttonValue.value = v
  handelModifying(true)
}

function previewIt() {
  emit('preview')
}

function handelModifying(modifying, reset = false) {
  // 取消操作，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.schedulingQuantity = v.sourceSchedulingQuantity // 还原
      return v
    })
  }
  emit('update:modifying', modifying)
}

// 应用
function applyAll() {
  if (crud.selections && crud.selections.length) {
    crud.selections.forEach((v) => {
      v.askCompleteTime = askCompleteTime.value
    })
  } else {
    ElMessage.warning('请至少选择一条数据进行应用')
  }
}

defineExpose({
  buttonValue,
  operateButtonEnum
})
</script>
