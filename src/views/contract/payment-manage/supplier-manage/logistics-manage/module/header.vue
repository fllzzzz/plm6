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
        style="width:240px"
      />
      <el-radio-group v-model="query.settlementStatus" size="small" class="filter-item"  @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button
          v-for="item in settlementStatusEnum.ENUM"
          :key="item.V"
          :label="item.V"
        >
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <el-input
        v-model="query.supplierName"
        placeholder="供应商搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model="query.orderSerialNumber"
        placeholder="订单号搜索"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
      <crudOperation>
        <!-- <template #optLeft>
          <el-tag v-if="totalSum" size="medium" class="filter-item">{{ `累计合同额:${totalSum.contractAmountSum?toThousand(totalSum.contractAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计结算额:${totalSum.settlementAmountSum?toThousand(totalSum.settlementAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计收款额:${totalSum.collectionAmountSum?toThousand(totalSum.collectionAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计开票额:${totalSum.invoiceAmountSum?toThousand(totalSum.invoiceAmountSum):'-'}元` }}</el-tag>
        </template> -->
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
// import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { settlementStatusEnum } from '@enum-ms/contract'
import { ElRadioGroup } from 'element-plus'
// import { ledgerSum } from '@/api/contract/contract-ledger'
// import { toThousand } from '@data-type/number'

const defaultQuery = {
  projectId: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  supplierName: undefined,
  orderSerialNumber: undefined,
  createTime: []
}

const { crud, query } = regHeader(defaultQuery)

// const totalSum = ref({})

// getSum()

// async function getSum() {
//   try {
//     const data = await ledgerSum()
//     totalSum.value = data || {}
//   } catch (e) {
//     console.log('获取累计金额', e)
//   }
// }
</script>
