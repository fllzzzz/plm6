<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
      <common-radio-button
        v-model="query.productType"
        :options="installProjectTypeEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="productTypeChange"
      />
      <!-- <project-subcontract-select
        v-model="query.projectId"
        style="width: 200px;margin-right:5px;"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="query.productType"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
       <common-select
        v-if="query.productType!==installProjectTypeEnum.AUXILIARY.V"
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="typeProp"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;"
        @change="crud.toQuery"
      />
    </div>
    <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get) && query.productType!==installProjectTypeEnum.AUXILIARY.V" :gutter="20" class="panel-group">
      <el-col :span="6" class="card-panel-col">
        <Panel name="清单总量" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.totalQuantity" is-array/>
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel name="已安装" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.totalInstall" is-array />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel name="剩余未安装" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.extraQuantity" is-array />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel name="安装率(%)" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.installRate || 0" :precision="2" />
      </el-col>
    </el-row>
    <crudOperation>
      <template #optLeft>
        <div>
          <el-input
            v-model.trim="query.supplierName"
            placeholder="供应商搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.serialNumber"
            placeholder="编号搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.name"
            placeholder="名称搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
      <template #viewLeft>
          <print-table
            v-permission="crud.permission.print"
            api-key="installReportList"
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
import { deliveryInstallSummary } from '@/api/project-manage/delivery-manage/delivery-report/report-list'
import { ref, watch, defineProps } from 'vue'

import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import { installProjectTypeEnum } from '@enum-ms/project'
import checkPermission from '@/utils/system/check-permission'

import monomerSelect from '@/components-system/plan/monomer-select'
import Panel from '../../../../components/Panel'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  productType: installProjectTypeEnum.ARTIFACT.V,
  monomerId: undefined,
  areaId: undefined,
  name: undefined,
  serialNumber: undefined,
  supplierName: undefined,
  areaType: undefined
}
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const areaInfo = ref([])
const totalAmount = ref({})
const summaryLoading = ref(false)

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

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
  if (!query.projectId || query.productType === installProjectTypeEnum.AUXILIARY.V) {
    return
  }
  summaryLoading.value = true
  try {
    const data = await deliveryInstallSummary(query)
    totalAmount.value = {
      ...data,
      installRate: (data.receivingQuantity && data.installQuantity) ? ((data.installQuantity / data.receivingQuantity) * 100) : 0,
      totalQuantity: [
        {
          quantity: data.quantity,
          unit: data.measureUnit,
          precision: 0
        },
        {
          quantity: data.mete,
          unit: data.accountingUnit,
          precision: 2
        }
      ],
      totalInstall: [
        {
          quantity: data.installQuantity,
          unit: data.measureUnit,
          precision: 0
        },
        {
          quantity: data.installMete,
          unit: data.accountingUnit,
          precision: 2
        }
      ],
      extraQuantity: [
        {
          quantity: data.receivingQuantity ? data.receivingQuantity - (data.installQuantity || 0) : 0,
          unit: data.measureUnit,
          precision: 0
        },
        {
          quantity: data.receivingMete ? data.receivingMete - (data.installMete || 0) : 0,
          unit: data.accountingUnit,
          precision: 2
        }
      ]
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

function getAreaInfo(val) {
  areaInfo.value = val || []
}

function productTypeChange(val) {
  query.areaId = undefined
  crud.toQuery()
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
