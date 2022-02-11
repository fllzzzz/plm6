<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query
        :basic-class="query.basicClass"
        :query="query"
        :to-query="crud.toQuery"
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
          <common-radio-button
            v-model="query.projectWarehouseType"
            :options="projectWarehouseTypeEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
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
        <common-radio-button v-model="query.unitType" :options="unitTypeEnum.ENUM" default text-align="center" type="enum" size="mini" />
      </template>
    </crudOperation>
    <template v-if="showAmount">
      <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel name="期初总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.beginPeriod || 0" :precision="2" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="入库总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.endPeriod || 0" :precision="2" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="出库总额（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.inbound || 0" :precision="2" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="期末结存（当月）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.outbound || 0" :precision="2" />
        </el-col>
      </el-row>
    </template>
  </div>
</template>

<script setup>
import { computed, inject, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { unitTypeEnum, projectWarehouseTypeEnum, orderSupplyTypeEnum } from '@/utils/enum/modules/wms'
import { isBlank } from '@/utils/data-type'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { specFormat, specTip } from '@/utils/wms/spec-format'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import Panel from '@/components/Panel'
import checkPermission from '@/utils/system/check-permission'

const permission = inject('permission')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0)])

const defaultQuery = {
  createTime: { value: `${new Date().getTime()}`, resetAble: false }, // [借用开始日期，借用结束日期]
  projectId: { value: undefined, resetAble: false }, // 原项目id
  factoryId: { value: undefined, resetAble: false }, // 工厂id
  basicClass: { value: undefined, resetAble: false }, // 基础类型
  orderSupplyType: { value: undefined, resetAble: false }, // 订单供应类型
  unitType: { value: unitTypeEnum.ACCOUNTING.V, resetAble: false } // 单位类型
}

const { CRUD, crud, query } = regHeader(defaultQuery)

// 全局项目id
const { globalProjectId } = mapGetters('globalProjectId')
const totalAmount = ref({})

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))

// 选中项目库时， 根据项目id的变化刷新列表
watch(
  globalProjectId,
  () => {
    if (isBlank(crud.query.projectWarehouseType) || crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
      crud.toQuery()
    }
  },
  { immediate: true }
)

CRUD.HOOK.beforeToQuery = () => {
  if (isBlank(crud.query.projectWarehouseType) || crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
    crud.query.projectId = globalProjectId.value || undefined
  } else {
    crud.query.projectId = undefined
  }
}

// 加载后数据处理
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  const { totalAmount: { beginPeriod = 0, endPeriod = 0, inbound = 0, outbound = 0 } = {}} = data
  totalAmount.value = {
    beginPeriod,
    endPeriod,
    inbound,
    outbound
  }
  // 设置规格等信息，多规格模式
  await setSpecInfoToList(data.content, { multipleSpec: true })
  const fmtFields = ['beginPeriod', 'endPeriod', 'inbound', 'outbound']
  for (const row of data.content) {
    fmtFields.forEach((k) => {
      numFmtByBasicClass(row[k], {
        basicClass: row.basicClass, // 基础分类
        measureUnit: row.measureUnit,
        accountingUnit: row.accountingUnit,
        accountingPrecision: row.accountingPrecision,
        measurePrecision: row.measurePrecision
      })
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
    // 规格提示信息
    row.specTip = specTip({ ...fmtObj, specificationLabels: row.specificationLabels })
  }
}

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}
</script>

<style lang="scss" scoped>
.panel-group {
  ::v-deep(.card-panel-description) {
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
</style>