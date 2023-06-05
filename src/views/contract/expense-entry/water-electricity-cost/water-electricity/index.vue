<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column v-if="columns.visible('date')" prop="date" key="date" :label="`${crud.query.year}年`" align="center" />
      <el-table-column
        v-if="columns.visible('usedMete')"
        align="center"
        key="usedMete"
        prop="usedMete"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '用电度数（kw/h）' : '用水量（吨）'"
      />
      <el-table-column
        v-if="columns.visible('totalAmount')"
        align="center"
        key="totalAmount"
        prop="totalAmount"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '电费（元）' : '水费（元）'"
      />
      <el-table-column
        v-if="columns.visible('averageValue')"
        align="center"
        key="averageValue"
        prop="averageValue"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '平均电费（元/kw·h）' : '平均单价（元/吨）'"
      />
      <el-table-column v-if="checkPermission([...permission.edit, ...permission.del])" align="center" label="操作" width="140px">
        <template #default="{ row }">
          <el-tag v-if="row.isAmortization" size="medium" type="success" effect="plain"> 已摊销 </el-tag>
          <udOperation v-else-if="row.isEdit" :data="row" />
          <udOperation v-else :data="row" :disabled-edit="true" :disabled-del="true" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/contract/expense-entry/water-electricity-cost'

import { waterElectricityCostPM as permission } from '@/page-permission/contract'
import { costTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import { toThousand } from '@/utils/data-type/number'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const tableRef = ref()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '水电费',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

// 合计
function getSummaries(param) {
  const data = tableSummary(param, {
    props: ['usedMete', 'totalAmount']
  })
  if (data[1] && data[2]) {
    data[3] = toThousand(data[2] / data[1])
  }
  if (data[1]) {
    data[1] = toThousand(data[1])
  }
  if (data[2]) {
    data[2] = toThousand(data[2])
  }
  return data
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  const length = data.content.length
  data.content = data.content.map((v, i) => {
    // 时间范围
    let _startDate = moment(v.startDate).format('YYYY')
    let _endDate = moment(v.endDate).format('YYYY')
    if (_startDate !== crud.query.year || _endDate !== crud.query.year) {
      _startDate = moment(v.startDate).format('YYYY-MM-DD')
      _endDate = moment(v.endDate).format('YYYY-MM-DD')
    } else {
      _startDate = moment(v.startDate).format('MM-DD')
      _endDate = moment(v.endDate).format('MM-DD')
    }
    v.date = `${_startDate} ~ ${_endDate}`
    // 最后一条记录才能编辑并且不能为已摊销状态
    v.isEdit = i + 1 === length && !v.isAmortization
    v.averageValue = v.totalAmount && v.usedMete ? (v.totalAmount / v.usedMete).toFixed(DP.YUAN) : 0
    return v
  })
}

const { maxHeight } = useMaxHeight()
</script>
