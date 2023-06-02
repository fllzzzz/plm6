<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-tooltip content="过滤采购合同" :show-after="100" placement="right">
        <span>
          <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            class="filter-item"
            style="width:100px!important"
            :disabled-date="disabledDate"
            placeholder="选择年"
            format="YYYY"
            value-format="YYYY"
            @change="crud.toQuery"
          />
        </span>
      </el-tooltip>
      <supplier-select
        v-model="query.supplierId"
        :type="supplierTypeEnum.RAW_MATERIAL.V"
        clearable
        class="filter-item"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 240px"
        @change="crud.toQuery"
        @info-change="getSupplierOrder"
      />
      <purchase-order-select
        class="filter-item"
        v-model="query.purchaseOrderId"
        :supplier-id="query.supplierId"
        :year="query.year"
        clearable
        style="width: 240px"
        @change="crud.toQuery"
        @info-change="getPurchaseOrder"
      />
      <el-date-picker
        v-model="query.inboundTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item"
        value-format="x"
        start-placeholder="入库日期"
        end-placeholder="入库日期"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        :disabled-date="disabledDate"
        style="width:240px"
        @change="crud.toQuery"
      />
      <material-cascader
        v-model="query.classifyId"
        separator=" > "
        check-strictly
        show-all-levels
        clearable
        size="small"
        class="filter-item"
        style="width: 300px"
        placeholder="可选择/输入科目、编号搜索"
        @change="crud.toQuery"
      />
      <rrOperation/>
      <div v-if="query.purchaseOrderId" v-loading="crud.loading">
        <el-tag class="tips" effect="plain" size="medium">采购合同编号：{{ purchaseOrder.serialNumber }}</el-tag>
        <el-tag v-for="item in projectList" :key="item.id" class="tips" effect="plain" size="medium">{{ item.shortName }} {{ item.serialNumber }}</el-tag>
      </div>
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag v-loading="crud.loading" class="tips" type="success" effect="plain" size="medium">总额（元）：{{ totalAmount }}</el-tag>
      </template>
      <template #viewLeft>
        <export-button v-permission="permission.download" class="tips" :params="query" :fn="excel" response-header-result>
          下载供应商对账（根据查询条件）
        </export-button>
      </template>
    </crudOperation>
    <el-row v-permission="permission.get" v-loading="summaryLoading" :gutter="20" class="panel-group">
      <el-col :span="8" class="card-panel-col">
        <Panel name="合同数（个）" text-color="#626262" num-color="#1890ff" :end-val="supplierSummary.totalOrder || 0" :precision="0" />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel name="合同额（万元）" text-color="#626262" num-color="#1890ff" :end-val="(supplierSummary.totalAmount / 10000) || 0" :precision="DP.YUAN" />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel name="实际收货额（万元）" text-color="#626262" num-color="#1890ff" :end-val="(supplierSummary.totalActualAmount / 10000) || 0" :precision="DP.YUAN" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { excel, summary } from '@/api/supply-chain/purchase-reconciliation-manage/reconciliation-log'
import { ref, inject } from 'vue'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { PICKER_OPTIONS_SHORTCUTS, STEEL_ENUM, DP } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { toThousand, getDP } from '@data-type/number'
import { specFormat, specTip } from '@/utils/wms/spec-format'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Panel from '@/components/Panel'
import ExportButton from '@comp-common/export-button/index.vue'
import purchaseOrderSelect from '@/components-system/wms/purchase-order-select/index.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  inboundTime: [], // [开始时间，结束时间]
  year: undefined,
  supplierId: undefined,
  classifyId: undefined,
  purchaseOrderId: undefined
}

const summaryData = {
  totalOrder: 0,
  totalAmount: 0,
  totalActualAmount: 0
}
const totalAmount = ref(0)
const purchaseOrder = ref({}) // 采购合同
const supplierOrder = ref({}) // 供应商合同
const projectList = ref([]) // 项目列表
const summaryLoading = ref(false)
const supplierSummary = ref({
  ...summaryData
}) // 供应商汇总信息
const permission = inject('permission')
const { CRUD, crud, query } = regHeader(defaultQuery)

function disabledDate(time) {
  return time > new Date()
}

// 采购合同信息
function getPurchaseOrder(order = {}, oldOrder) {
  purchaseOrder.value = order
}

// 供应商合同信息
function getSupplierOrder(order = {}, oldOrder) {
  supplierOrder.value = order
}

// 获取供应商汇总
async function fetchSummary() {
  try {
    summaryLoading.value = true
    const data = await summary({
      year: query.year || undefined,
      supplierId: query.supplierId
    }) || {}
    supplierSummary.value = data
  } catch (error) {
    supplierSummary.value = {
      ...summaryData
    }
    console.log('供应商汇总', error)
  } finally {
    summaryLoading.value = false
  }
}

// 重新查询前
CRUD.HOOK.beforeToQuery = () => {
  if (!query.inboundTime) {
    query.inboundTime = undefined
  }
  if (!query.year) {
    query.year = undefined
  }
}

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  // 汇总金额
  totalAmount.value = toThousand(data.supplierReconciliationAmount) || 0
  projectList.value = data.project || []
  data.content = await setSpecInfoToList(data.content)

  data.content.forEach(row => {
    if (row.basicClass < STEEL_ENUM) {
      // 此页面钢材默认显示吨，保留3位
      row.accountingUnit = '吨'
      row.accountingPrecision = 3
    }

    // --------------------- 处理规格 -------------------------
    /**
   * 规格展示
   * 钢板和钢卷：只展示厚度和材质（规格配置）
   * 其他：只展示材质（规格配置）
   */

    // 规格格式转换
    const fmtObj = {
      basicClass: row.basicClass, // 基础分类
      thickness: row.thickness, // 厚度
      specification: row.specification // 规格
    }
    row.formatSpec = specFormat(fmtObj)
    // 规格提示信息
    row.specTip = specTip({ ...fmtObj, specificationLabels: row.specificationLabels })
  })

  data.content = await numFmtByBasicClass(data.content, { toNum: true })
  data.content.forEach(v => {
    v.unitPriceExcludingVAT = toThousand(v.unitPriceExcludingVAT, getDP(v.unitPriceExcludingVAT))
  })
  fetchSummary()
}
</script>

<style lang="scss" scoped>
  .tips + .tips {
    margin-left: 6px;
  }
  .panel-group {
    ::v-deep(.card-panel) {
      .card-panel-description {
        margin: 10px 20px;
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        align-items: flex-start;
        flex-wrap: wrap;
        .card-panel-text {
          margin-top: 2px;
        }
        .card-panel-num {
          font-size: 20px;
        }
      }
    }
  }
</style>
