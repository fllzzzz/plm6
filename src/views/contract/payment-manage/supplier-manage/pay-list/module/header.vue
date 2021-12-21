<template>
  <div>
    <div v-show="crud.searchToggle">
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
        v-model="query.paymentReason"
        :options="dict.payment_reason"
        type="dict"
        clearable
        size="small"
        placeholder="付款事由"
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.payForType"
        :options="contractPayForEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="费用类别"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input v-model="query.paymentUnit" placeholder="付款单位" style="width: 200px" class="filter-item" />
      <el-input v-model="query.receiveUnit" placeholder="收款单位" style="width: 200px" class="filter-item" />
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
import { defineProps, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { supplierPayMentTypeEnum, auditTypeEnum, contractPayDateTypeEnum, contractPayForEnum } from '@enum-ms/contract'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import { getContentInfo } from '@/api/contract/project'
import useDict from '@compos/store/use-dict'
import { paySum } from '@/api/contract/supplier-manage/pay-invoice/pay'
import { toThousand } from '@/utils/data-type/number'

const dict = useDict(['payment_reason'])
const defaultQuery = {
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  basicClass: undefined,
  orderSerialNumber: undefined,
  paymentUnit: undefined,
  propertyType: undefined,
  receiveUnit: undefined,
  supplierName: undefined,
  paymentReason: undefined,
  payForType: undefined,
}

const { CRUD, crud, query } = regHeader(defaultQuery)
const totalSum = ref()

getPaySum()

async function getPaySum() {
  let data = undefined
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
