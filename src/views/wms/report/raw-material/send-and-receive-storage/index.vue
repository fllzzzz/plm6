<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`return_to_party_a_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :cell-style="handleCellStyle"
      :header-cell-style="handleCellStyle"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      highlight-current-row
      @row-click="handleRowClick"
      @sort-change="crud.handleSortChange"
    >
      <!-- 基础信息 -->
      <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
        <template #default="{ row, $index }">
          <!-- 是否甲供材料 -->
          <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classifySerialNumber')"
        prop="classifySerialNumber"
        key="classifySerialNumber"
        label="物料编号"
        align="center"
        width="110px"
        fixed="left"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('classification')"
        prop="classification"
        key="classification"
        label="分类"
        align="center"
        width="100"
        fixed="left"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('classifyName')"
        prop="classifyName"
        key="classifyName"
        label="名称"
        align="center"
        show-overflow-tooltip
        :width="classifyNameWidth"
        fixed="left"
      >
        <template #default="{ row }">
          <!-- 半出余料标签 -->
          <table-cell-tag :show="!!row.boolOddmentByHalfOut" name="半出余料" color="#e6a23c" />
          <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
            {{ row.classifyName }}
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        label="规格"
        min-width="200"
        align="left"
        fixed="left"
      >
        <template #default="{ row }">
          <div class="spec-box">
            <el-tooltip :content="row.specTip" placement="top">
              <span class="spec-info ellipsis-text" :style="{ 'padding-right': row.multipleSpec ? '20px' : 0 }">
                {{ row.formatSpecArr }}
              </span>
            </el-tooltip>
            <el-popover
              v-if="row.multipleSpec"
              placement="top"
              :title="`${row.classifyName}（${row.specTip}）【${row.sourceFormatSpecArr.length}种】`"
              :width="500"
              trigger="click"
            >
              <template #reference>
                <span class="icon expand-click-8 pointer">
                  <svg-icon icon-class="more" style="flex: none; color: #2c2c2c" />
                </span>
              </template>
              <template #default>
                <div style="margin-top: 20px">
                  <template v-for="(spec, index) in row.sourceFormatSpecArr" :key="spec">
                    <span style="font-weight: bold">{{ spec }}</span>
                    <span v-if="index < row.sourceFormatSpecArr.length - 1">&nbsp;&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;</span>
                  </template>
                </div>
              </template>
            </el-popover>
          </div>
        </template>
      </el-table-column>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <el-table-column
        v-if="columns.visible('currentUnit')"
        key="currentUnit"
        prop="currentUnit"
        show-overflow-tooltip
        label="单位"
        width="70px"
        align="center"
        fixed="left"
      />
      <el-table-column v-if="showProject" prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip />
      <el-table-column
        v-if="!crud.query.factoryId && columns.visible('factory.name')"
        key="factory.name"
        prop="factory.name"
        show-overflow-tooltip
        label="工厂"
        min-width="100px"
      />
      <!-- ############################ 期初数据 ############################ -->
      <el-table-column key="beginPeriodData" prop="beginPeriodData" label="期初数据" align="center">
        <el-table-column
          v-if="columns.visible('beginPeriod.number')"
          key="beginPeriod.number"
          prop="beginPeriod.number"
          show-overflow-tooltip
          label="数量"
          width="100px"
          align="right"
        />
        <template v-if="showAmount">
          <el-table-column
            v-if="showUnitPrice && columns.visible('beginPeriod.unitPriceExcludingVAT')"
            key="beginPeriod.unitPriceExcludingVAT"
            prop="beginPeriod.unitPriceExcludingVAT"
            show-overflow-tooltip
            label="单价(不含税)"
            width="100px"
            align="right"
          />
          <el-table-column
            v-if="columns.visible('beginPeriod.amountExcludingVAT')"
            key="beginPeriod.amountExcludingVAT"
            prop="beginPeriod.amountExcludingVAT"
            show-overflow-tooltip
            label="金额(不含税)"
            width="100px"
            align="right"
          />
        </template>
      </el-table-column>
      <!-- ############################ 入库数据 ############################ -->
      <el-table-column key="inboundData" prop="inboundData" label="入库数据" align="center">
        <el-table-column
          v-if="columns.visible('inbound.number')"
          key="inbound.number"
          prop="inbound.number"
          show-overflow-tooltip
          label="数量"
          width="100px"
          align="right"
        />
        <template v-if="showAmount">
          <el-table-column
            v-if="showUnitPrice && columns.visible('inbound.unitPriceExcludingVAT')"
            key="inbound.unitPriceExcludingVAT"
            prop="inbound.unitPriceExcludingVAT"
            show-overflow-tooltip
            label="单价(不含税)"
            width="100px"
            align="right"
          />
          <el-table-column
            v-if="columns.visible('inbound.amountExcludingVAT')"
            key="inbound.amountExcludingVAT"
            prop="inbound.amountExcludingVAT"
            show-overflow-tooltip
            label="金额(不含税)"
            width="100px"
            align="right"
          />
        </template>
      </el-table-column>
      <!-- ############################ 出库数据 ############################ -->
      <el-table-column key="outboundData" prop="outboundData" label="出库数据" align="center">
        <el-table-column
          v-if="columns.visible('outbound.number')"
          key="outbound.number"
          prop="outbound.number"
          show-overflow-tooltip
          label="数量"
          width="100px"
          align="right"
        />
        <template v-if="showAmount">
          <el-table-column
            v-if="showUnitPrice && columns.visible('outbound.unitPriceExcludingVAT')"
            key="outbound.unitPriceExcludingVAT"
            prop="outbound.unitPriceExcludingVAT"
            show-overflow-tooltip
            label="单价(不含税)"
            width="100px"
            align="right"
          />
          <el-table-column
            v-if="columns.visible('outbound.amountExcludingVAT')"
            key="outbound.amountExcludingVAT"
            prop="outbound.amountExcludingVAT"
            show-overflow-tooltip
            label="金额(不含税)"
            width="100px"
            align="right"
          />
        </template>
      </el-table-column>
      <!-- ######################## 期末数据 ############################ -->
      <el-table-column key="endPeriodData" prop="endPeriodData" label="期末数据" align="center">
        <el-table-column
          v-if="columns.visible('endPeriod.number')"
          key="endPeriod.number"
          prop="endPeriod.number"
          show-overflow-tooltip
          label="数量"
          width="100px"
          align="right"
        />
        <template v-if="showAmount">
          <el-table-column
            v-if="showUnitPrice && columns.visible('endPeriod.unitPriceExcludingVAT')"
            key="endPeriod.unitPriceExcludingVAT"
            prop="endPeriod.unitPriceExcludingVAT"
            show-overflow-tooltip
            label="单价(不含税)"
            width="100px"
            align="right"
          />
          <el-table-column
            v-if="columns.visible('endPeriod.amountExcludingVAT')"
            key="endPeriod.amountExcludingVAT"
            prop="endPeriod.amountExcludingVAT"
            show-overflow-tooltip
            label="金额(不含税)"
            width="100px"
            align="right"
          />
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页组件 -->
    <!-- <Pagination /> -->
    <!-- 详情 -->
    <MDetail v-model:visible="detailVisible" :material-info="currentRow" :date="crud.query.createTime" />
  </div>
</template>

<script setup>
import { getSendAndReceiveStorage, getSendAndReceiveStorageDetail } from '@/api/wms/report/raw-material/statistics'

import { computed, ref, watch } from 'vue'
import { reportRawMaterialSendAndReceiveStoragePM as permission } from '@/page-permission/wms'
import { measureTypeEnum, projectWarehouseTypeEnum, unitTypeEnum } from '@/utils/enum/modules/wms'
import { STEEL_ENUM } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
// import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import MDetail from './module/detail'

import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import checkPermission from '@/utils/system/check-permission'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['project', ['parse-project', { onlyShortName: true }]],
  ['beginPeriod.unitPriceExcludingVAT', 'to-thousand'],
  ['beginPeriod.amountExcludingVAT', 'to-thousand'],
  ['inbound.unitPriceExcludingVAT', 'to-thousand'],
  ['inbound.amountExcludingVAT', 'to-thousand'],
  ['outbound.unitPriceExcludingVAT', 'to-thousand'],
  ['outbound.amountExcludingVAT', 'to-thousand'],
  ['endPeriod.unitPriceExcludingVAT', 'to-thousand'],
  ['endPeriod.amountExcludingVAT', 'to-thousand'],
  ['formatSpecArr', 'split']
])

// 展开行
const expandRowKeys = ref([])
// 当前行
const currentRow = ref({})
// 详情显示
const detailVisible = ref(false)
// 表格ref
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '收发存报表',
    sort: ['id.desc'],
    invisibleColumns: ['classifySerialNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    hasPagination: false,
    crudApi: { get: getSendAndReceiveStorage, detail: getSendAndReceiveStorageDetail }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const showProject = computed(
  // 未选择项目 && 当前仓库不为公共仓 && 显示项目
  () =>
    !crud.query.projectId &&
    crud.query.projectWarehouseType &&
    crud.query.projectWarehouseType !== projectWarehouseTypeEnum.PUBLIC.V &&
    columns.value.visible('project')
)

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))

// 显示单价
const showUnitPrice = computed(() => crud.query.unitType === measureTypeEnum.ACCOUNTING.V)

// 物料全名宽度
const classifyNameWidth = computed(() => {
  // 基础分类不存在，或基础分类不为钢材，则宽度为100
  return !crud.query.basicClass || crud.query.basicClass > STEEL_ENUM ? 160 : 120
})

// 监听，处理数据
watch(
  [() => crud.data, () => crud.query.unitType],
  ([data, unitType]) => {
    if (data && data.length > 0) {
      unitType = data.forEach((row) => {
        let measureType
        switch (unitType) {
          case unitTypeEnum.MEASURE.V:
            if (row.measureUnit) {
              measureType = measureTypeEnum.MEASURE.V
            } else {
              measureType = measureTypeEnum.ACCOUNTING.V
            }
            break
          case unitTypeEnum.ACCOUNTING.V:
            measureType = measureTypeEnum.ACCOUNTING.V
            break
          case unitTypeEnum.OUTBOUND.V:
            measureType = row.outboundUnitType
            break
        }
        if (!row.beginPeriod) row.beginPeriod = {}
        if (!row.endPeriod) row.endPeriod = {}
        if (!row.inbound) row.inbound = {}
        if (!row.outbound) row.outbound = {}
        if (measureType === measureTypeEnum.MEASURE.V) {
          row.currentUnit = row.measureUnit
          row.beginPeriod.number = row.beginPeriod.quantity
          row.endPeriod.number = row.endPeriod.quantity
          row.inbound.number = row.inbound.quantity
          row.outbound.number = row.outbound.quantity
        } else {
          row.currentUnit = row.accountingUnit
          row.beginPeriod.number = row.beginPeriod.mete
          row.endPeriod.number = row.endPeriod.mete
          row.inbound.number = row.inbound.mete
          row.outbound.number = row.outbound.mete
        }
      })
    }
  },
  { immediate: true, deep: true }
)

function handleCellStyle({ row, column, rowIndex, columnIndex }) {
  if (column.property && column.property.indexOf('beginPeriod') !== -1) {
    return 'background-color:#f5ffef;'
  }
  if (column.property && column.property.indexOf('inbound') !== -1) {
    return 'background-color:#fff9e9;'
  }
  if (column.property && column.property.indexOf('outbound') !== -1) {
    return 'background-color:#fbf1eb;'
  }
  if (column.property && column.property.indexOf('endPeriod') !== -1) {
    return 'background-color:#e5eaf4;'
  }
}

// 表格行点击
function handleRowClick(row, column, event) {
  currentRow.value = row
  detailVisible.value = true
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(td .cell) {
    min-height: 28px;
    line-height: 28px;
  }
}
.spec-box {
  position: relative;
  .spec-info {
    display: inline-block;
    width: 100%;
    padding-right: 20px;
  }
  .icon {
    position: absolute;
    right: 0;
    font-size: 18px;
    .svg-icon {
      color: #409eff !important;
    }
  }
}
</style>
