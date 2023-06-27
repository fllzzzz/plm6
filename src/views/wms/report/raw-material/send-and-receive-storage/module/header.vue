<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query
        :basic-class="query.basicClass"
        :query="query"
        :to-query="crud.toQuery"
        :show-basic-class-query="false"
        :show-warehouse="false"
        :show-material-is-whole="false"
      >
        <template #firstItem>
          <el-date-picker
            v-model="query.createTime"
            :default-time="defaultTime"
            type="month"
            range-separator=":"
            size="small"
            value-format="x"
            unlink-panels
            :clearable="false"
            placeholder="查询月份"
            :disabled-date="disabledDate"
            style="width: 135px"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template #afterWarehouse>
          <common-radio-button
            v-model="query.orderSupplyType"
            :options="orderSupplyTypeEnum.ENUM"
            show-option-all
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <warehouse-project-cascader
            v-model:projectId="query.projectId"
            v-model:projectWarehouseType="query.projectWarehouseType"
            class="filter-item"
            @change="crud.toQuery"
          />
          <common-radio-button
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
      </mat-header-query>
      <rrOperation />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft>
        <!-- <common-radio-button v-model="query.unitType" :options="unitTypeEnum.ENUM" default text-align="center" type="enum" size="mini" /> -->
      </template>
      <template #viewLeft>
        <export-button v-permission="permission.get" :params="query" :fn="exportSendAndReceiveStorageExcel" response-header-result>
          下载收发存报表（根据查询条件）
        </export-button>
      </template>
    </crudOperation>
    <template v-if="showAmount">
      <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel name="期初总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.beginPeriod || 0" :precision="decimalPrecision.wms" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="入库总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.inbound || 0" :precision="decimalPrecision.wms" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="出库总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.outbound || 0" :precision="decimalPrecision.wms" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="期末结存（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.endPeriod || 0" :precision="decimalPrecision.wms" />
        </el-col>
      </el-row>
    </template>
  </div>
</template>

<script setup>
import { exportSendAndReceiveStorageExcel } from '@/api/wms/report/raw-material/statistics'
import { computed, inject, ref, defineExpose } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { unitTypeEnum, orderSupplyTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import Panel from '@/components/Panel'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'
import ExportButton from '@comp-common/export-button/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const permission = inject('permission')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0)])

const defaultQuery = {
  createTime: { value: `${new Date().getTime()}`, resetAble: false }, // 时间
  projectId: { value: undefined, resetAble: false }, // 原项目id
  factoryId: { value: undefined, resetAble: false }, // 工厂id
  projectWarehouseType: undefined, // 仓库类型
  basicClass: { value: undefined, resetAble: false }, // 基础类型
  orderSupplyType: { value: undefined, resetAble: false }, // 订单供应类型
  unitType: { value: unitTypeEnum.ACCOUNTING.V, resetAble: false } // 单位类型
}

const { CRUD, crud, query } = regHeader(defaultQuery)

const weightedType = ref()
const totalAmount = ref({})

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))

function disabledDate(time) {
  return time > new Date()
}

// 加载后数据处理
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  const { weightedType: _weightedType, totalAmount: { beginPeriod = 0, endPeriod = 0, inbound = 0, outbound = 0 } = {}} = data
  weightedType.value = _weightedType
  totalAmount.value = {
    beginPeriod,
    endPeriod,
    inbound,
    outbound
  }
  // 设置规格等信息，多规格模式
  await setSpecInfoToList(data.content, { multipleSpec: true })
  const fmtFields = ['beginPeriod', 'endPeriod', 'inbound', 'outbound']
  const allPs = []
  for (const row of data.content) {
    fmtFields.forEach((k) => {
      const ps = numFmtByBasicClass(row[k], {
        basicClass: row.basicClass, // 基础分类
        measureUnit: row.measureUnit,
        accountingUnit: row.accountingUnit,
        accountingPrecision: row.accountingPrecision,
        measurePrecision: row.measurePrecision
      })
      allPs.push(ps)
    })
    // --------------------- 处理规格 -------------------------
    row.multipleSpec = Array.isArray(row.specifications) && row.specifications.length > 1
    // 规格格式转换
    const formatSpecArr = []
    const fmtObj = {
      basicClass: row.basicClass, // 基础分类
      thickness: row.thickness, // 厚度
      width: row.width, // 宽度
      length: row.length, // 长度
      color: row.color // 颜色
    }
    if (Array.isArray(row.specifications)) {
      for (const spec of row.specifications) {
        fmtObj.specification = spec // 规格
        const specFmt = specFormat(fmtObj)
        formatSpecArr.push(specFmt)
      }
    } else {
      const specFmt = specFormat(fmtObj)
      formatSpecArr.push(specFmt)
    }

    row.formatSpecArr = formatSpecArr
    row.sourceFormatSpecArr = [...formatSpecArr]
    // 规格提示信息
    row.specTip = specTip({ ...fmtObj, specificationLabels: row.specificationLabels })
  }
  await Promise.all(allPs)
}

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}

defineExpose({
  weightedType
})
</script>

<style lang="scss" scoped>
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
