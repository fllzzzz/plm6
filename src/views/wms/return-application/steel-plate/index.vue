<template>
  <div class="app-container">
    <div class="head-container">
      <div class="filter-container">
        <div class="filter-left-box"></div>
        <div class="filter-right-box">
          <common-button type="success" @click="openReturnableList">检索退库材料</common-button>
        </div>
      </div>
    </div>
    <div class="main-content">
      <common-table ref="tableRef" :data="list" :max-height="maxHeight" :default-expand-all="false" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
        <el-table-column prop="project" label="项目" align="left" min-width="120px" fixed="left" show-overflow-tooltip>
          <template #default="{ row }">
            <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
        <el-table-column prop="classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
        <el-table-column prop="specification" label="规格" align="center" width="100px" fixed="left">
          <template #default="{ row }">
            <el-tooltip :content="row.specificationLabels" placement="top">
              <span v-empty-text>{{ row.specification }}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" fixed="left" />
        <el-table-column prop="thickness" align="center" width="100px" :label="`厚 (mm)`" fixed="left">
          <template #default="{ row }">
            <span v-to-fixed="baseUnit.thickness.precision">{{ row.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="width" align="center" width="110px" :label="`宽 (mm)`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.width"
              :min="0"
              :max="999999"
              controls-position="right"
              :controls="false"
              :precision="0"
              size="mini"
              placeholder="宽"
            />
          </template>
        </el-table-column>
        <el-table-column prop="length" align="center" width="110px" :label="`长 (mm)`">
          <template #default="{ row }">
            <el-input-number v-model="row.length" :max="999999" :controls="false" :min="0" :precision="0" size="mini" placeholder="长" />
          </template>
        </el-table-column>
        <el-table-column prop="quantity" align="center" width="110px" :label="`数量 (${baseUnit.measure.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.quantity"
              :min="1"
              :max="999999999"
              controls-position="right"
              :controls="false"
              :step="1"
              :precision="0"
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
              v-model="row.weighingTotalWeight"
              :min="0"
              :max="999999999"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.weight.precision"
              size="mini"
              placeholder="重量"
            />
            <!-- </el-tooltip> -->
          </template>
        </el-table-column>
        <warehouse-set-columns :list="list" />
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template #default="{ $index }">
            <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow($index)" />
          </template>
        </el-table-column>
      </common-table>
    </div>
    <returnable-list-drawer v-model="returnableVisible" :basic-class="basicClass" :select-list="list" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import ReturnableListDrawer from '@/views/wms/return-application/components/returnable-list-drawer/index.vue'
// import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
// import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
// import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
// import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

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

// 打开
function openReturnableList() {
  returnableVisible.value = true
}

function delRow(index) {
  list.value.splice(index, 1)
}
</script>
