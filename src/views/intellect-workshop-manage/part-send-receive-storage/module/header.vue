<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
        style="width:120px;"
      />
      <common-radio-button
        v-model="query.status"
        :options="intellectWorkshopStatusEnum.ENUM"
        showOptionAll
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        size="small"
        placeholder="项目名称"
        style="width: 200px;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <rrOperation />
      <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
        <el-col :span="8" class="card-panel-col">
          <Panel name="累计入库(t)" text-color="#626262" num-color="#1890ff" :endVal="totalAmount.intoWeight || 0" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="累计出库(t)" text-color="#626262" num-color="#1890ff" :endVal="totalAmount.outWeight || 0" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="当前库存(t)" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.stockWeight || 0" />
        </el-col>
      </el-row>
    </div>
    <crudOperation>
       <template #viewLeft>
          <!-- <print-table
            v-permission="crud.permission.print"
            api-key="deliveryInstallList"
            :params="{ ...query, queryType: 1 }"
            size="mini"
            type="warning"
            class="filter-item"
          /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { summaryData } from '@/api/intellect-workshop-manage/receive-send-storage'
import { ref, watch } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { intellectWorkshopStatusEnum } from '@enum-ms/intellect-workshop'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Panel from '@/components/Panel'

const defaultQuery = {
  year: undefined,
  status: undefined,
  name: undefined,
  productType: componentTypeEnum.MACHINE_PART.V
}

const { crud, query } = regHeader(defaultQuery)
const totalAmount = ref({})
const summaryLoading = ref(false)

watch(
  query,
  (val) => {
    if (val) {
      fetchSummaryInfo()
    }
  },
  { immediate: true, deep: true }
)
async function fetchSummaryInfo() {
  summaryLoading.value = true
  try {
    const data = await summaryData(query)
    totalAmount.value = data
    totalAmount.value.intoWeight = (totalAmount.value.intoWeight / 1000).toFixed(DP.COM_WT__T)
    totalAmount.value.outWeight = (totalAmount.value.outWeight / 1000).toFixed(DP.COM_WT__T)
    totalAmount.value.stockWeight = (totalAmount.value.stockWeight / 1000).toFixed(DP.COM_WT__T)
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:left;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 17px;
        text-align:right;
      }
    }
  }
}
</style>
