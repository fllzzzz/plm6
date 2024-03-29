<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.dateTime"
        type="year"
        size="small"
        class="date-item filter-item"
        placeholder="选择年"
        format="YYYY"
        value-format="x"
        @change="crud.toQuery"
        style="width: 120px"
      />
      <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.productType"
        :options="[packTypeEnum.BOX]"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <workshop-select
        v-model="query.workShopId"
        placeholder="请选择车间"
        clearable
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
        <el-col :span="8" class="card-panel-col">
          <Panel
            name="累计入库(t)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="totalAmount.inboundMete / 1000 || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel
            name="累计出库(t)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="totalAmount.outboundMete / 1000 || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel
            name="实时库存(t)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="totalAmount.stockMete / 1000 || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
      </el-row>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="bridgeProductSendReceiveStorage"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { summaryData } from '@/api/bridge/bridge-pack-and-ship/product-receive-send-storage'
import { ref, watch } from 'vue'
import workshopSelect from '@comp-mes/workshop-select'
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import Panel from '@/components/Panel'

const defaultQuery = {
  dateTime: undefined,
  projectId: undefined,
  productType: packTypeEnum.BOX.V,
  workShopId: undefined
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
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align: left;
        margin-top: 2px;
      }
      .card-panel-num {
        display: block;
        font-size: 17px;
        text-align: right;
      }
    }
  }
}
</style>
