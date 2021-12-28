<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="75%"
    title="计量配置"
    :wrapper-closable="false"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button v-show="isEditMode" size="mini" type="primary" @click="preview">保存预览</common-button>
      <common-button v-show="isEditMode" size="mini" type="danger" @click="handleCancelEdit">退出编辑</common-button>
      <common-button v-show="!isEditMode" size="mini" type="primary" @click="isEditMode = true">编辑</common-button>
    </template>
    <template #content>
      <div class="class-unit-config">
        <div class="query-content">
          <el-input
            v-model.trim="query.nameOrCode"
            placeholder="输入科目名称、编号搜索"
            class="filter-item"
            style="width: 200px;margin-bottom: 15px;"
            size="small"
            clearable
          />
        </div>
        <common-table
          ref="table"
          v-loading="loading"
          border
          :data="filterList"
          :cell-class-name="changedCellMask"
          :max-height="maxHeight"
          :highlight-current-row="false"
          :span-method="spanMethod"
          row-key="id"
        >
          <el-table-column type="index" label="序号" align="center" width="60" />
          <el-table-column key="first" prop="first" label="一级" align="left" min-width="160px">
            <template #header>
              <el-tooltip effect="light" :content="`一级科目（名称-编号）`" placement="top">
                <div>
                  <span>一级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <span class="classify-name">{{ `${scope.row.fullName[0]}-${scope.row.fullCode[0]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="level > 1" key="second" prop="second" label="二级" align="left" min-width="160px">
            <template v-slot:header>
              <el-tooltip effect="light" :content="`二级科目（名称-编号）`" placement="top">
                <div>
                  <span>二级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <span class="classify-name">{{ `${scope.row.fullName[1]}-${scope.row.fullCode[1]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="level > 2" key="third" prop="third" label="三级" align="left" min-width="160px">
            <template v-slot:header>
              <el-tooltip effect="light" :content="`三级科目（名称-编号）`" placement="top">
                <div style="margin: 0 10px">
                  <span>三级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <span class="classify-name">{{ `${scope.row.fullName[2]}-${scope.row.fullCode[2]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="measureUnit"
            prop="measureUnit"
            :show-overflow-tooltip="true"
            label="计量单位"
            min-width="100px"
            align="center"
          >
            <template v-slot="scope">
              <unit-select
                v-if="isEditMode"
                v-model="scope.row.measureUnit"
                size="mini"
                style="width: 100%"
                :disabled="scope.row.disabled"
                clearable
                filterable
              />
              <div v-else>{{ emptyTextFormatter(scope.row.sourceMeasureUnit) }}</div>
            </template>
          </el-table-column>
          <el-table-column
            key="measurePrecision"
            prop="measurePrecision"
            :show-overflow-tooltip="true"
            label="小数精度(计量)"
            min-width="100px"
            align="center"
          >
            <template v-slot="scope">
              <el-input-number
                v-if="isEditMode"
                v-model="scope.row.measurePrecision"
                :step="1"
                :min="0"
                :max="10"
                :disabled="scope.row.disabled"
                size="mini"
                style="width: 100%"
              />
              <div v-else>{{ emptyTextFormatter(scope.row.sourceMeasurePrecision) }}</div>
            </template>
          </el-table-column>
          <el-table-column
            key="accountingUnit"
            prop="accountingUnit"
            :show-overflow-tooltip="true"
            label="核算单位"
            min-width="100px"
            align="center"
          >
            <template v-slot="scope">
              <unit-select
                v-if="isEditMode"
                v-model="scope.row.accountingUnit"
                :unit-type="scope.row.basicClass & STEEL_ENUM > 0 ? unitTypeEnum.WEIGHT.K : undefined"
                :disabled="scope.row.disabled"
                size="mini"
                style="width: 100%"
                clearable
                filterable
              />
              <div v-else>{{ emptyTextFormatter(scope.row.sourceAccountingUnit) }}</div>
            </template>
          </el-table-column>
          <el-table-column
            key="accountingPrecision"
            prop="accountingPrecision"
            :show-overflow-tooltip="true"
            label="小数精度(核算)"
            min-width="100px"
            align="center"
          >
            <template v-slot="scope">
              <el-input-number
                v-if="isEditMode"
                v-model="scope.row.accountingPrecision"
                :step="1"
                :min="0"
                :max="10"
                :disabled="scope.row.disabled"
                size="mini"
                style="width: 100%"
              />
              <div v-else>{{ emptyTextFormatter(scope.row.sourceAccountingPrecision) }}</div>
            </template>
          </el-table-column>
          <el-table-column key="outboundUnitType" prop="outboundUnitType" :show-overflow-tooltip="true" label="出库单位" min-width="100px" align="center">
        <template v-slot="scope">
          <common-select
            v-if="isEditMode"
            v-model="scope.row.outboundUnitType"
            :options="measureTypeEnum.ENUM"
            :disabled="scope.row.disabled"
            text-align="center"
            type="enum"
            size="mini"
            style="width: 100%;"
            clearable
          />
          <div v-else>{{ emptyTextFormatter(measureTypeEnum.VL[scope.row.outboundUnitType]) }}</div>
        </template>
      </el-table-column>
        </common-table>
        <config-preview v-model:visible="previewVisible" :data="list" :level="level" @save-success="handleSaveSuccess" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, inject, reactive, ref, watch, computed } from 'vue'
import { emptyTextFormatter } from '@data-type'
import { STEEL_ENUM } from '@/settings/config'
import { measureTypeEnum } from '@enum-ms/wms'
import { unitTypeEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableChange from '@compos/form/use-table-change'
import unitSelect from '@comp-common/unit-select'
import configPreview from './config-preview'

const emit = defineEmits(['update:visible'])

const props = defineProps({
  classificationList: {
    type: Array,
    default: () => []
  },
  level: {
    type: Number,
    default: 2
  },
  visible: {
    type: Boolean,
    default: false
  }
})

const sourceMap = inject('sourceMap')

const drawerRef = ref()
const list = ref([])
const query = reactive({
  nameOrCode: ''
})

const previewVisible = ref(false)
const isEditMode = ref(false)
const loading = ref(false)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: handleCancelEdit })

const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.common-drawer',
    extraBox: ['.el-drawer__header', '.query-content'],
    wrapperBox: ['.el-drawer__body', '.class-unit-config'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

// 根据搜索条件过滤
const filterList = computed(() => {
  const nameOrCode = query.nameOrCode
  const _list = list.value.filter(
    (v) => v.fullName.some((v) => v.indexOf(nameOrCode) > -1) || v.fullSerialNumber.some((v) => v.indexOf(nameOrCode) > -1)
  )
  return mergeCells(_list)
})

watch(
  [() => props.visible, () => props.classificationList, () => props.level],
  ([visible]) => {
    if (visible) {
      init()
    }
  },
  { immediate: true }
)

function init() {
  loading.value = true
  // 深拷贝数组，退出编辑时还原数据
  list.value = props.classificationList.map((item) => {
    return { ...item, disabled: (item.basicClass & STEEL_ENUM) > 0 }
  })
  loading.value = false
}

// 退出编辑
function handleCancelEdit() {
  isEditMode.value = false
  init()
}

// 预览窗口
function preview() {
  previewVisible.value = true
}

function mergeCells(list) {
  if (list.length === 0) return list
  const row = [[], []]
  const id = [-1, -1]
  list.forEach((v) => {
    for (let i = 0; i < row.length; i++) {
      const newId = v.fullId[i]
      const oldId = id.length > i ? id[i] : undefined
      if (newId === oldId) {
        row[i][row[i].length - 1]++
      } else {
        row[i].push(1)
        id[i] = newId
      }
    }
  })
  row[0].reduce((total, cur) => {
    list[total].name1_rowSpan = cur
    total += cur
    return total
  }, 0)
  row[1].reduce((total, cur) => {
    list[total].name2_rowSpan = cur
    total += cur
    return total
  }, 0)
  return list
}

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (props.level > 1) {
    if (column.property === 'first') {
      return {
        rowspan: row.name1_rowSpan || 0,
        colspan: 1
      }
    }
    if (props.level > 2 && column.property === 'second') {
      return {
        rowspan: row.name2_rowSpan || 0,
        colspan: 1
      }
    }
  }
}

function handleSaveSuccess() {
  isEditMode.value = false
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

.classify-name {
  padding: 0 10px;
}
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 32px;
    padding:0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
