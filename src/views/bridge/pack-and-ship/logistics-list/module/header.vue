<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.manufactureType"
        :options="manufactureTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <component-radio-button
        v-model="query.productType"
        :options="packTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-if="query.productType & packTypeEnum.ENCLOSURE.V"
        v-model="query.category"
        :options="mesEnclosureTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.priceType"
        :options="logisticsPriceTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.deliveryDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="发运开始日期"
        end-placeholder="发运结束日期"
        style="width: 240px"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        @change="handleDeliveryDateChange"
      />
      <div>
        <el-input
          v-model="query.supplierName"
          placeholder="可输入物流公司搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入车次搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="可输入车牌搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.driverName"
          placeholder="可输入司机搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="permission.print"
          api-key="bridgeLogisticsSummary"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
      <template #viewLeft>
        <common-button v-permission="permission.edit" @click="feeVisible = true" size="mini" type="primary">物流费设置</common-button>
      </template>
    </crudOperation>
  </div>
  <logistics-fee-setting v-model:visible="feeVisible" />
</template>

<script setup>
import { inject, ref } from 'vue'
import moment from 'moment'

import { packTypeEnum, mesEnclosureTypeEnum, logisticsPriceTypeEnum } from '@enum-ms/mes'
import { manufactureTypeEnum } from '@enum-ms/production'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import logisticsFeeSetting from './logistics-fee-setting/index'

const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  driverName: undefined,
  auditUserName: undefined,
  auditStartDate: undefined,
  auditEndDate: undefined,
  auditReceiptName: undefined,
  receiptName: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false },
  manufactureType: { value: undefined, resetAble: false },
  supplierName: undefined,
  priceType: undefined
}
const { crud, query } = regHeader(defaultQuery)

const permission = inject('permission')
const feeVisible = ref(false)

function handleDeliveryDateChange() {
  if (query.deliveryDate && query.deliveryDate.length > 1) {
    query.auditStartDate = moment(query.deliveryDate[0]).valueOf()
    query.auditEndDate = moment(query.deliveryDate[1]).valueOf()
  } else {
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
  crud.toQuery()
}
</script>
