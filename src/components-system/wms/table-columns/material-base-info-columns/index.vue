<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" :fixed="fixed">
    <template #default="{ row, $index }">
      <!-- 是否甲供材料 -->
      <table-cell-tag v-if="showPartyA" :show="!!row.boolPartyA" name="甲供" :color="TAG_PARTY_DEF_COLOR" />
      <span>{{ $index + 1 }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showSerialNumber" prop="serialNumber" label="编号" align="center" width="110px" :fixed="fixed">
    <template #default="{ row }">
      <!-- 甲供调拨方式 -->
      <table-cell-tag
        v-if="showPartyATransfer && row.partyATransferType"
        :name="partyAMatTransferEnum.VL[row.partyATransferType]"
        :color="partyAMatTransferEnum.V[row.partyATransferType].COLOR"
        :offset="15"
      />
      <!-- 出库方式 -->
      <table-cell-tag
        v-if="showOutboundMode && row.materialOutboundMode === materialOutboundModeEnum.HALF.V"
        :name="materialOutboundModeEnum.VL[row.materialOutboundMode]"
        :color="materialOutboundModeEnum.V[row.materialOutboundMode].COLOR"
        :offset="15"
      />
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <!-- 钢材宽度100， 其他180 :min-width="basicClass > STEEL_ENUM ? 180 : undefined" -->
  <el-table-column
    v-if="showClassifyFullName"
    prop="classifyFullName"
    label="物料种类"
    align="center"
    show-overflow-tooltip
    :width="classifyFullNameWidth"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <!-- 是否显示冻结角标 -->
      <span v-if="showFrozenTip && row.boolHasFrozen" class="table-cell-triangle-frozen" />
      <!-- 是否可以查看材料冻结 -->
      <span v-if="frozenViewable && row.boolHasFrozen" class="freeze-text" v-empty-text @click="openMatFrozenDetail(row)">
        {{ row.classifyFullName }}
      </span>
      <!-- 正常显示 -->
      <span v-else v-empty-text>{{ row.classifyFullName }}</span>
    </template>
  </el-table-column>
  <component :is="comp" :columns="columns" :basic-class="basicClass" :spec-merge="specMerge" :fixed="fixed" />
  <common-dialog
    :title="`冻结记录：${currentMaterial.classifyFullName} ${currentMaterial.specification}`"
    v-model="freezeDialogVisible"
    width="1300px"
    show-close
    :before-close="handleFreezeClose"
    custom-class="wms-material-freeze-record-view"
    top="10vh"
  >
    <material-freeze-record :material="currentMaterial" :max-height="freezeMaxHeight" @unfreeze-success="handleUnfreezeSuccess" />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, computed, ref } from 'vue'
import { STEEL_ENUM, TAG_PARTY_DEF_COLOR } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { materialOutboundModeEnum, partyAMatTransferEnum } from '@/utils/enum/modules/wms'
import { isBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'
import materialFreezeRecord from '@/views/wms/freeze-manage/components/material-freeze-record.vue'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'
import useMaxHeight from '@/composables/use-max-height'

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
  showIndex: {
    // 显示 “序号”
    type: Boolean,
    default: true
  },
  showPartyA: {
    // 显示 “甲供”
    type: Boolean,
    default: true
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
  fixed: {
    // 定位
    type: String
  }
})

const permission = {
  frozenDetail: ['wms_raw_mat_freeze:detail']
}

// 当前物料
const currentMaterial = ref({})
// 冻结记录窗口显示状态
const freezeDialogVisible = ref(false)
// 操作次数(列如冻结)
const operateNumber = ref(0)

// 物料全名宽度
const classifyFullNameWidth = computed(() => {
  // 基础分类不存在，或基础分类不为钢材，则宽度为100
  return !props.basicClass || props.basicClass > STEEL_ENUM ? 250 : 120
})

// 是否显示物料种类全路径
const showClassifyFullName = computed(() => isBlank(props.columns) || props.columns.visible('classifyFullName'))
// 是否显示编号
const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible('serialNumber'))
// 可查看冻结信息
const frozenViewable = computed(() => props.frozenViewable && checkPermission(permission.frozenDetail))

// 冻结表格高度
const { maxHeight: freezeMaxHeight } = useMaxHeight(
  {
    mainBox: '.wms-material-freeze-record-view',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  freezeDialogVisible
)

// 打开冻结详情
function openMatFrozenDetail(row) {
  operateNumber.value = 0
  freezeDialogVisible.value = true
  currentMaterial.value = row
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

<style lang="scss" scoped>
.freeze-text {
  color: #409eff;
  cursor: pointer;
}
</style>
