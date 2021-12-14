<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.productType"
      :options="reportComponentTypeEnum.ENUM"
      showOptionAll
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 300px"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #viewLeft>
      <span v-if="query.productType" v-permission="permission.get">
        <el-tag effect="plain" style="cursor: pointer" class="filter-item" @click="showDetail(reportTypeEnum.BEGIN.V)">
          <span>期初库存：</span>
          <span v-if="!summaryLoading">{{ summaryInfo.beginMete }}</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
        <el-tag effect="plain" style="cursor: pointer" class="filter-item" @click="showDetail(reportTypeEnum.INBOUND.V)">
          <span>入库量：</span>
          <span v-if="!summaryLoading">{{ summaryInfo.inboundMete }}</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
        <el-tag effect="plain" style="cursor: pointer" class="filter-item" @click="showDetail(reportTypeEnum.OUTBOUND.V)">
          <span>出库量：</span>
          <span v-if="!summaryLoading">{{ summaryInfo.outboundMete }}</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
        <el-tag effect="plain" style="cursor: pointer" class="filter-item" type="success" @click="showDetail(reportTypeEnum.END.V)">
          <span>期末库存：</span>
          <span v-if="!summaryLoading">{{ summaryInfo.endMete }}</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
      </span>
    </template>
  </crudOperation>
</template>

<script setup>
import { getSummary } from '@/api/mes/manufactures-manage/report'
import { inject, defineEmits, ref } from 'vue'
import moment from 'moment'

import { reportComponentTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf(),
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false }
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const emit = defineEmits(['showDetail'])
const reportTypeEnum = inject('reportTypeEnum')
const permission = inject('permission')

function showDetail(type) {
  emit('showDetail', type)
}

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

const summaryLoading = ref(false)
const summaryInfo = ref({})

CRUD.HOOK.beforeToQuery = () => {
  fetchSummaryInfo()
}

async function fetchSummaryInfo() {
  if (!checkPermission(permission.get) || !query.productType) {
    return
  }
  summaryLoading.value = true
  summaryInfo.value = {}
  try {
    const { beginMete = 0, inboundMete = 0, outboundMete = 0, endMete = 0 } = await getSummary(query)
    summaryInfo.value = {
      beginMete,
      inboundMete,
      outboundMete,
      endMete
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}
</script>
