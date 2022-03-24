<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" :fixed="fixed">
    <template #default="{ row, $index }">
      <div v-if="row.overTipColor" class="left-triangle-tip" :style="{ 'border-left-color': row.overTipColor }" />
      <table-cell-tag v-if="showPartyA && partyAPosition === 'index'" :show="!!getInfo(row, 'boolPartyA')" name="甲供" type="partyA" />
      <span>{{ $index + 1 }}</span>
    </template>
  </el-table-column>
  <slot name="afterIndex" />
  <el-table-column
    v-if="showProject"
    :prop="`${field}.project`"
    label="项目"
    align="left"
    min-width="120px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <table-cell-tag
        v-if="showPartyA && partyAPosition === 'project'"
        :show="!!getInfo(row, 'boolPartyA')"
        name="甲供"
        type="partyA"
        :offset="15"
      />
      {{ getInfo(row, 'project') }}
    </template>
  </el-table-column>
  <el-table-column
    v-if="showSerialNumber"
    :prop="`${field}.serialNumber`"
    label="物料编号"
    align="center"
    width="110px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <table-cell-tag
        v-if="showPartyATransfer && getInfo(row, 'partyATransferType')"
        :name="partyAMatTransferEnum.VL[getInfo(row, 'partyATransferType')]"
        :color="partyAMatTransferEnum.V[getInfo(row, 'partyATransferType')].COLOR"
        :offset="15"
      />
      <table-cell-tag
        v-if="showOutboundMode && getInfo(row, 'materialOutboundMode') === materialOutboundModeEnum.HALF.V"
        :name="materialOutboundModeEnum.VL[getInfo(row, 'materialOutboundMode')]"
        :color="materialOutboundModeEnum.V[getInfo(row, 'materialOutboundMode')].COLOR"
        :offset="15"
      />
      <!-- 显示退货状态 -->
      <template v-if="showRejectStatus">
        <table-cell-tag
          v-if="isNotBlank(getInfo(row, 'rejectStatus')) && getInfo(row, 'rejectStatus') !== materialRejectStatusEnum.NONE.V"
          :name="materialRejectStatusEnum.VL[getInfo(row, 'rejectStatus')]"
          :color="materialRejectStatusEnum.V[getInfo(row, 'rejectStatus')].COLOR"
          :class="{ pointer: rejectDetailViewable }"
          @click="openMatRejectDetail(row)"
        />
      </template>
      <span>{{ getInfo(row, 'serialNumber') }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showClassifyName"
    :key="`${field}.classifyName`"
    :prop="`${field}.classifyName`"
    label="物料种类"
    align="center"
    show-overflow-tooltip
    :width="classifyNameWidth"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <!-- 是否显示冻结角标 -->
      <span v-if="showFrozenTip && getInfo(row, 'boolHasFrozen')" class="table-cell-triangle-frozen" />
      <el-tooltip
        :content="getInfo(row, 'classifyParentFullName')"
        :disabled="!getInfo(row, 'classifyParentFullName')"
        :show-after="500"
        placement="top"
      >
        <span>
          <!-- 是否可以查看材料冻结 -->
          <span v-if="frozenViewable && getInfo(row, 'boolHasFrozen')" class="freeze-text" @click="openMatFrozenDetail(row)">
            {{ getInfo(row, 'classifyName') }}
          </span>
          <!-- 正常显示 -->
          <span v-else>{{ getInfo(row, 'classifyName') }}</span>
        </span>
      </el-tooltip>
    </template>
  </el-table-column>
  <component
    :is="comp"
    :columns="props.columns"
    :basic-class="props.basicClass"
    :spec-merge="props.specMerge"
    :fixed="fixed"
    :field="field"
    v-bind="$attrs"
  />

  <!-- 冻结记录 -->
  <common-dialog
    :title="`冻结记录：${currentMaterial.classifyName} ${currentMaterial.specification}`"
    v-model="freezeDialogVisible"
    width="1300px"
    show-close
    :before-close="handleFreezeClose"
    custom-class="wms-material-freeze-record-view"
    top="10vh"
  >
    <material-freeze-record :material="currentMaterial" :max-height="maxHeight" @unfreeze-success="handleUnfreezeSuccess" />
  </common-dialog>

  <!-- 退货详情 -->
  <common-dialog
    :title="`退货记录：${currentMaterial.classifyName} ${currentMaterial.specification}`"
    v-model="rejectMaterialDialogVisible"
    width="80%"
    show-close
    custom-class="wms-material-reject-material-record-view"
    top="10vh"
  >
    <reject-info-table
      :material="currentMaterial"
      :basic-class="currentMaterial.basicClass"
      :list="currentMaterial.rejectList"
      :max-height="maxHeight"
      has-border
    />
  </common-dialog>
</template>

<script setup>
import { materialBaseInfoCPM as permission } from '@/page-permission/wms'

import { defineEmits, defineProps, computed, provide, ref } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { materialRejectStatusEnum, materialOutboundModeEnum, partyAMatTransferEnum } from '@/utils/enum/modules/wms'
import { isBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@/composables/use-max-height'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'

import RejectInfoTable from '@/views/wms/material-reject/raw-material/components/reject-info-table.vue'
import materialFreezeRecord from '@/views/wms/material-freeze/raw-material/components/material-freeze-record.vue'

const emit = defineEmits(['refresh'])

const props = defineProps({
  specMerge: {
    // 规格合并,规格与 厚宽长颜色等合并为一个字段
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showFrozenTip: {
    // 显示冻结角标
    type: Boolean,
    default: false
  },
  frozenViewable: {
    // 可查看冻结
    type: Boolean,
    default: false
  },
  rejectDetailViewable: {
    // 可查看退货详情
    type: Boolean,
    default: false
  },
  showIndex: {
    // 显示 “序号”
    type: Boolean,
    default: true
  },
  showRejectStatus: {
    // 显示 “退货状态”
    type: Boolean,
    default: false
  },
  showPartyA: {
    // 显示 “甲供”
    type: Boolean,
    default: true
  },
  partyAPosition: {
    type: String,
    default: 'index' // index / project
  },
  showPartyATransfer: {
    // 显示 甲供调拨类型
    type: Boolean,
    default: false
  },
  showOutboundMode: {
    // 显示 出库方式 （整料半出）
    type: Boolean,
    default: false
  },
  showProject: {
    // 显示项目
    type: Boolean,
    default: false
  },
  fixed: {
    // 定位
    type: String
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

// 当前物料
const currentMaterial = ref({})
// 冻结记录窗口显示状态
const freezeDialogVisible = ref(false)
// 退货详情窗口显示状态
const rejectMaterialDialogVisible = ref(false)
// 操作次数(列如冻结)
const operateNumber = ref(0)

// 物料全名宽度
const classifyNameWidth = computed(() => {
  // 基础分类不存在，或基础分类不为钢材，则宽度为100
  return !props.basicClass || props.basicClass > STEEL_ENUM ? 150 : 120
})

// 是否显示物料种类全路径
const showClassifyName = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.classifyName`))
// 是否显示编号
const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.serialNumber`))
// 可查看冻结信息
const frozenViewable = computed(() => props.frozenViewable && checkPermission(permission.frozenDetail))

// 表格高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-material-freeze-record-view',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  computed(() => freezeDialogVisible.value || rejectMaterialDialogVisible.value)
)

// 打开冻结详情
function openMatFrozenDetail(row) {
  const sourceRow = row.sourceRow ? row.sourceRow : row
  operateNumber.value = 0
  currentMaterial.value = getInfo(sourceRow)
  freezeDialogVisible.value = true
}

// 打开退货详情
function openMatRejectDetail(row) {
  if (!props.rejectDetailViewable) return
  const sourceRow = row.sourceRow ? row.sourceRow : row
  currentMaterial.value = getInfo(sourceRow)
  rejectMaterialDialogVisible.value = true
}

// 解冻成功
function handleUnfreezeSuccess() {
  ++operateNumber.value
}

// 关闭窗口
function handleFreezeClose(done) {
  if (operateNumber.value > 0) {
    emit('refresh')
  }
  done()
}

// 根据传入的物料字段获取信息
function getInfo(row, field) {
  const materialField = props.field
  if (isBlank(row) || isBlank(row[materialField])) return
  return !field ? row[materialField] : row[materialField][field]
}
provide('getInfo', getInfo)

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return rawMat
  }
})
</script>
