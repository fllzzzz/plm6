<template>
  <div class="app-container">
    <div class="head-container">
      <material-info class="filter-item" :basic-class="basicClass" :material="currentSource" />
      <div class="filter-container">
        <div class="filter-left-box">
          <span class="total-info">
            <span class="info-item">
              <span>总件数({{ baseUnit.measure.unit }})</span>
              <span v-to-fixed="{ val: AllQuantity || 0, dp: baseUnit.measure.precision }" />
            </span>
            <span class="info-item">
              <span>总重量({{ baseUnit.weight.unit }})</span>
              <span v-to-fixed="{ val: AllMete || 0, dp: baseUnit.weight.precision }" />
            </span>
          </span>
        </div>
        <div class="filter-right-box">
          <common-button type="success" @click="openReturnableList">检索退库材料</common-button>
        </div>
      </div>
    </div>
    <div class="main-content">
      <common-table
        ref="tableRef"
        :data="list"
        :max-height="maxHeight"
        :default-expand-all="false"
        :stripe="false"
        highlight-current-row
        row-key="uid"
        @row-click="handleRowClick"
      >
        <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
        <el-table-column prop="source.project" label="项目" align="left" min-width="120px" fixed="left" show-overflow-tooltip>
          <template #default="{ row }">
            <span v-parse-project="{ project: row.source.project, onlyShortName: true }" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column prop="source.serialNumber" label="编号" align="center" width="110px" fixed="left" />
        <el-table-column prop="source.classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
        <el-table-column prop="source.specification" label="规格" align="center" width="100px" fixed="left">
          <template #default="{ row }">
            <el-tooltip :content="row.source.specificationLabels" placement="top">
              <span v-empty-text>{{ row.source.specification }}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" field="source" fixed="left" />
        <el-table-column prop="source.thickness" align="center" width="100px" :label="`厚 (${baseUnit.thickness.unit})`" fixed="left">
          <template #default="{ row }">
            <span v-to-fixed="baseUnit.thickness.precision">{{ row.source.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="width" align="center" width="110px" :label="`宽 (${baseUnit.width.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.width"
              :min="0"
              :max="+row.source.width"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.width.precision"
              size="mini"
              placeholder="宽"
            />
          </template>
        </el-table-column>
        <el-table-column prop="length" align="center" width="110px" :label="`长 (${baseUnit.length.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.length"
              :max="+row.source.length"
              :controls="false"
              :min="0"
              :precision="baseUnit.length.precision"
              size="mini"
              placeholder="长"
            />
          </template>
        </el-table-column>
        <el-table-column prop="quantity" align="center" width="110px" :label="`数量 (${baseUnit.measure.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.quantity"
              :min="1"
              :max="+row.source.quantity"
              controls-position="right"
              :controls="false"
              :step="1"
              :precision="baseUnit.measure.precision"
              size="mini"
              placeholder="数量"
            />
          </template>
        </el-table-column>
        <el-table-column
          key="weighingTotalWeight"
          prop="weighingTotalWeight"
          align="center"
          :label="`总重 (${baseUnit.weight.unit})`"
          width="120px"
        >
          <template #default="{ row }">
            <!-- <el-tooltip
              class="item"
              effect="dark"
              :content="`理论重量：${row.theoryTotalWeight} kg， ${overDiffTip}`"
              :disabled="!row.hasOver"
              placement="top"
            > -->
            <el-input-number
              v-model="row.mete"
              :min="0"
              :max="row.maxTotalWeight"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.weight.precision"
              size="mini"
              placeholder="重量"
            />
            <!-- </el-tooltip> -->
            <!-- <span v-to-fixed="baseUnit.weight.precision">{{ row.mete }}</span> -->
          </template>
        </el-table-column>
        <warehouse-set-columns :list="list" />
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template #default="{ row, $index }">
            <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row, $index)" />
          </template>
        </el-table-column>
      </common-table>
    </div>
    <returnable-list-drawer v-model="returnableVisible" :basic-class="basicClass" :select-list="list" @add="handleAdd" />
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { isNotBlank, toFixed } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import ReturnableListDrawer from '@/views/wms/return-application/components/returnable-list-drawer/index.vue'
import MaterialInfo from '@/views/wms/return-application/components/material-info/index.vue'

// import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
// import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
// import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
// import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

const tableRef = ref()
// 归还列表
const list = ref([])
// 显示可归还列表
const returnableVisible = ref(false)
// 最大高度
const { maxHeight } = useMaxHeight({ paginate: false })
// 钢板类型
const basicClass = rawMatClsEnum.STEEL_PLATE.V
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(basicClass)

// 当前退库源数据
const currentSource = ref()
// 当前高亮uid
const currentUid = ref()
// 总量
const AllMete = ref()
// 总数量
const AllQuantity = ref()

// 监听list
watch(
  list,
  () => {
    setDefaultCurrent()
  },
  { deep: true }
)

// 打开
function openReturnableList() {
  returnableVisible.value = true
}

// 行选中
function handleRowClick(row, column, event) {
  currentUid.value = row.uid
  currentSource.value = row.source
}

// 删除当前行
function delRow(row, index) {
  list.value.splice(index, 1)
  if (row.uid === currentUid.value || !currentUid.value) {
    setTimeout(() => {
      if (list.value.length > 0) {
        // delRow后会触发handleRowClick,因此延迟触发
        const newCurrent = index < list.value.length ? list.value[index] : list.value[index - 1]
        tableRef.value.setCurrentRow(newCurrent)
        handleRowClick(newCurrent)
      } else {
        // 数据为空时，选中设置为null
        currentUid.value = null
        currentSource.value = null
      }
    }, 0)
  }
}

// 设置默认选中行
function setDefaultCurrent() {
  if (list.value.length > 0 && !currentUid.value) {
    // 选中第一行
    const newCurrent = list.value[0]
    tableRef.value.setCurrentRow(newCurrent)
    handleRowClick(newCurrent)
  }
}

// 添加材质
function handleAdd(row) {
  // 计算最大总重
  watch([() => row.quantity], () => {
    calcMaxTotalWeight(row)
    calcAllQuantity()
  })
  // 计算理论及单重
  watch([() => row.length, () => row.width, baseUnit], () => calcTheoryWeight(row))
  // 计算总重
  watch([() => row.singleMete, () => row.quantity], () => {
    calcTotalWeight(row)
    calcAllWeight()
  })

  // 钢材总重计算
  // watch(
  //   () => row.weighingTotalWeight,
  //   () => {
  //     emit('calc-weight')
  //   },
  //   { immediate: true }
  // )
}

// 计算单件理论重量
async function calcTheoryWeight(row) {
  row.theoryWeight = await calcSteelPlateWeight({
    name: row.source.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
    length: row.length,
    width: row.width,
    thickness: row.source.thickness
  })
  if (row.theoryWeight) {
    row.singleMete = +toFixed((row.theoryWeight / row.source.theoryWeight) * row.source.singleReturnableMete)
  } else {
    row.singleMete = undefined
  }
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.singleMete) && row.quantity) {
    row.mete = +toFixed(row.singleMete * row.quantity, baseUnit.value.weight.precision)
  } else {
    row.mete = undefined
  }
}

// 计算最大重量
function calcMaxTotalWeight(row) {
  if (row.quantity) {
    row.maxTotalWeight = row.source.singleReturnableMete * row.quantity
  } else {
    row.maxTotalWeight = row.source.singleReturnableMete
  }
}

function calcAllWeight() {
  AllMete.value = list.value.reduce((sum, { mete = 0 }) => {
    return +toFixed(sum + mete, baseUnit.value.weight.precision)
  }, 0)
}

function calcAllQuantity() {
  AllQuantity.value = list.value.reduce((sum, { quantity = 0 }) => {
    return +toFixed(sum + quantity, baseUnit.value.measure.precision)
  }, 0)
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(.current-row > td.el-table__cell) {
    --el-table-current-row-background-color: #d7ffef;
  }
}

.filter-left-box {
  width: 100%;
}

.total-info {
  padding: 5px 0;
  .info-item {
    display: inline-block;
    margin: 5px 0;
    font-size: 13px;
    min-width: 180px;
    // color: brown;
    // font-weight: bold;
    // width: 100px;
    > span {
      display: inline-block;
      overflow: hidden;
    }
    > span:first-child {
      font-weight: bold;
      width: 80px;
      text-align: right;
      &:after {
        content: '：';
      }
    }
    > span:last-child {
      text-overflow: ellipsis;
      white-space: nowrap;
      // display: inline;
      // color: #ccc;
      // font-weight: normal;
    }
  }
}
</style>
