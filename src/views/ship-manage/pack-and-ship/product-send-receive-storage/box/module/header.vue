<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.dateTime"
        type="month"
        size="small"
        class="date-item filter-item"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="x"
        @change="crud.toQuery"
        style="width: 120px"
      />
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button v-model="query.productType" :options="bridgeProductTypeEnum.ENUM" type="enum" class="filter-item" @change="crud.toQuery" />
      <!-- <common-radio-button
        v-model="query.productType"
        :options="[packTypeEnum.STRUCTURE, packTypeEnum.MACHINE_PART]"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <workshop-select
        v-model="query.workshopId"
        :workshop-type="workshopTypeEnum.BRIDGE.V"
        placeholder="请选择车间"
        clearable
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        type="enum"
        v-model="query.weightStatus"
        :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get)" :gutter="24" class="panel-group">
        <el-col :span="6" class="card-panel-col" style="cursor: pointer;" @click="inventoryDetails(1)">
          <Panel
            name="期初库存(kg)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="query.weightStatus === weightTypeEnum.NET.V? totalAmount?.beginningNetWeight || 0 : totalAmount?.beginningGrossWeight || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col" style="cursor: pointer;" @click="inventoryDetails(2)">
          <Panel
            name="入库量(kg)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="query.weightStatus === weightTypeEnum.NET.V?totalAmount?.inboundNetWeight || 0:totalAmount?.inboundGrossWeight || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col" style="cursor: pointer;" @click="inventoryDetails(3)">
          <Panel
            name="出库量(kg)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="query.weightStatus === weightTypeEnum.NET.V?totalAmount?.outboundNetWeight || 0:totalAmount?.outboundGrossWeight || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col" style="cursor: pointer;" @click="inventoryDetails(4)">
          <Panel
            name="期末库存(kg)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="query.weightStatus === weightTypeEnum.NET.V?totalAmount?.stockNetWeight || 0:totalAmount?.stockGrossWeight || 0"
            :precision="DP.COM_WT__KG"
          />
        </el-col>
      </el-row>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          :api-key="query.productType===bridgeProductTypeEnum.BOX.V?'boxProductSendReceiveStorage':'boxProductSendReceiveStorageDirect'"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
    <detail-drawer v-model:showDetailDrawer="showDetailDrawer" :query="crud.query" :inventoryType="inventoryType"  />
  </div>
</template>

<script setup>
import { summaryData } from '@/api/ship-manage/pack-and-ship/box-product-receive-send-storage'
import { ref, watch } from 'vue'
// import { packTypeEnum } from '@enum-ms/mes'
import { bridgeProductTypeEnum } from '@enum-ms/ship-manage'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import Panel from '@/components/Panel'
import moment from 'moment'
import detailDrawer from './detail-drawer'

const defaultTime = moment().valueOf().toString()

const defaultQuery = {
  productType: bridgeProductTypeEnum.BOX.V,
  dateTime: defaultTime.toString(),
  projectId: undefined,
  weightStatus: weightTypeEnum.NET.V
}

const { crud, query } = regHeader(defaultQuery)
const totalAmount = ref({})
const summaryLoading = ref(false)
const showDetailDrawer = ref(false)
const inventoryType = ref()

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
    // totalAmount.value.intoWeight = (totalAmount.value.intoWeight / 1000).toFixed(DP.COM_WT__T)
    // totalAmount.value.outWeight = (totalAmount.value.outWeight / 1000).toFixed(DP.COM_WT__T)
    // totalAmount.value.stockWeight = (totalAmount.value.stockWeight / 1000).toFixed(DP.COM_WT__T)
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

const inventoryDetails = (v) => {
  showDetailDrawer.value = true
  inventoryType.value = v
  console.log(v)
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
