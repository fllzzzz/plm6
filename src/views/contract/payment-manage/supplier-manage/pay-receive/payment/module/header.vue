<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.dateType"
        :options="contractPayDateTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="日期类型"
        style="width: 110px"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 200px"
      />
      <el-input v-model="query.orderSerialNumber" placeholder="订单号" style="width: 200px" class="filter-item" />
      <el-input v-model="query.supplierName" placeholder="供应商" style="width: 200px" class="filter-item" />
      <common-select
        v-model="query.propertyType"
        :options="supplierPayMentTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="属性"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <basic-class-select
        v-model="query.basicClass"
        clearable
        placeholder="物料种类"
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.auditStatus"
        :options="auditTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="状态"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input v-model="query.paymentUnit" placeholder="付款单位" style="width: 200px" class="filter-item" />
      <el-input v-model="query.receiveUnit" placeholder="收款单位" style="width: 200px" class="filter-item" />
      <el-input v-model="query.writtenByName" placeholder="填报人" style="width: 200px" class="filter-item" />
      <el-input v-model="query.auditUserName" placeholder="审核人" style="width: 200px" class="filter-item" />
      <rrOperation />
      <crudOperation add-text="付款填报">
        <template #viewLeft>
          <el-tag type="success" v-if="totalSum" size="medium">{{ `累计付款金额：${totalSum.toThousand()}元` }}</el-tag>
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { supplierPayMentTypeEnum, auditTypeEnum, contractPayDateTypeEnum } from '@enum-ms/contract'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import { paySum } from '@/api/contract/supplier-manage/pay-invoice/pay'

const defaultQuery = {
  dateType: contractPayDateTypeEnum.ENUM.UPDATE_DATE.V,
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  writtenByName: undefined,
  auditStatus: undefined,
  auditUserName: undefined,
  basicClass: undefined,
  orderSerialNumber: undefined,
  paymentUnit: undefined,
  propertyType: undefined,
  receiveUnit: undefined,
  supplierName: undefined
}

const { CRUD, crud, query } = regHeader(defaultQuery)
const totalSum = ref()

getPaySum()

async function getPaySum() {
  let data
  try {
    data = await paySum()
  } catch (e) {
    console.log('累计付款', e)
  } finally {
    totalSum.value = data
  }
}
CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime && crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  }
}
</script>
