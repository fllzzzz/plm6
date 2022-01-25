<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" :fixed="fixed">
    <template #default="{ row, $index }">
      <!-- 是否甲供材料 -->
      <table-cell-tag v-if="showPartyA" :show="!!row.boolPartyA" name="甲供" :color="TAG_PARTY_DEF_COLOR" />
      <span>{{ $index + 1 }}</span>
    </template>
  </el-table-column>
  <slot name="afterIndex" />
  <el-table-column v-if="showProject" :prop="`project`" label="项目" align="left" min-width="120px" :fixed="fixed" show-overflow-tooltip>
    <template #default="{ row }">
      <table-cell-tag
        v-if="showPartyA && partyAPosition === 'project'"
        :show="!!row.boolPartyA"
        name="甲供"
        :color="TAG_PARTY_DEF_COLOR"
        :offset="15"
      />
      <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column
    v-if="showSerialNumber"
    prop="serialNumber"
    label="物料编号"
    align="center"
    width="110px"
    :fixed="fixed"
    show-overflow-tooltip
    :sortable="sortable"
  >
    <template #default="{ row }">
      <!-- 甲供调拨方式 -->
      <table-cell-tag
        v-if="showPartyATransfer && row.partyATransferType"
        :name="partyAMatTransferEnum.VL[row.partyATransferType]"
        :color="partyAMatTransferEnum.V[row.partyATransferType].COLOR"
        :offset="15"
      />
      <!-- 物料类型 与 出库方式同时存在-->
      <!-- <template v-if="showIsWhole && showOutboundMode">
        <table-cell-tag
          v-if="row.materialIsWhole === materialIsWholeEnum.ODDMENT.V || row.materialOutboundMode === materialOutboundModeEnum.HALF.V"
          :name="`${materialIsWholeEnum.VL[row.materialIsWhole]}${
            row.materialOutboundMode === materialOutboundModeEnum.HALF.V ? materialOutboundModeEnum.VL[row.materialOutboundMode] : ''
          }`"
          :color="
            row.materialIsWhole === materialIsWholeEnum.ODDMENT.V && row.materialOutboundMode === materialOutboundModeEnum.HALF.V
              ? '#e66f3c'
              : materialIsWholeEnum.V[row.materialIsWhole].COLOR
          "
          :offset="15"
        />
      </template>
      <template v-else> -->
      <!-- 物料类型 整料/余料 -->
      <table-cell-tag
        v-if="showIsWhole && row.materialIsWhole === materialIsWholeEnum.ODDMENT.V"
        :name="materialIsWholeEnum.VL[row.materialIsWhole]"
        :color="materialIsWholeEnum.V[row.materialIsWhole].COLOR"
        :offset="15"
      />
      <!-- 出库方式 -->
      <table-cell-tag
        v-if="showOutboundMode && row.materialOutboundMode === materialOutboundModeEnum.HALF.V"
        :name="materialOutboundModeEnum.VL[row.materialOutboundMode]"
        :color="materialOutboundModeEnum.V[row.materialOutboundMode].COLOR"
        :offset="15"
      />
      <!-- </template> -->

      <!-- 显示退货状态 -->
      <template v-if="showRejectStatus">
        <table-cell-tag
          v-if="isNotBlank(row.rejectStatus) && row.rejectStatus !== materialRejectStatusEnum.NONE.V"
          :name="materialRejectStatusEnum.VL[row.rejectStatus]"
          :color="materialRejectStatusEnum.V[row.rejectStatus].COLOR"
          :class="{ pointer: rejectDetailViewable }"
          @click="openMatRejectDetail(row)"
        />
      </template>
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showClassifyName"
    prop="classifyName"
    label="物料种类"
    align="center"
    show-overflow-tooltip
    :width="classifyNameWidth"
    :fixed="fixed"
    :sortable="sortable"
  >
    <template #default="{ row }">
      <!-- 是否显示冻结角标 -->
      <span v-if="showFrozenTip && row.boolHasFrozen" class="table-cell-triangle-frozen" />
      <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
        <span>
          <!-- 是否可以查看材料冻结 -->
          <span
            v-if="frozenViewable && row.boolHasFrozen"
            class="freeze-text"
            v-empty-text="row.classifyName"
            @click="openMatFrozenDetail(row)"
          />
          <!-- 正常显示 -->
          <span v-else v-empty-text="row.classifyName" />
        </span>
      </el-tooltip>
    </template>
  </el-table-column>
  <component v-bind="$attrs" :is="comp" :columns="columns" :basic-class="basicClass" :spec-merge="specMerge" :fixed="fixed" />

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
import { defineEmits, defineProps, computed, ref } from 'vue'
import { STEEL_ENUM, TAG_PARTY_DEF_COLOR } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { materialRejectStatusEnum, materialIsWholeEnum, materialOutboundModeEnum, partyAMatTransferEnum } from '@/utils/enum/modules/wms'
import { isNotBlank, isBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@/composables/use-max-height'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'

import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'
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
  showIndex: {
    // 显示 “序号”
    type: Boolean,
    default: true
  },
  showProject: {
    // 显示项目
    type: Boolean,
    default: false
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
  showPartyATransfer: {
    // 显示 甲供调拨类型
    type: Boolean,
    default: false
  },
  showIsWhole: {
    // 显示 材料类型 （整料|余料）
    type: Boolean,
    default: false
  },
  showOutboundMode: {
    // 显示 出库方式 （整出|半出）
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
  rejectDetailViewable: {
    // 可查看退货详情
    type: Boolean,
    default: false
  },
  fixed: {
    // 定位
    type: String
  },
  sortable: {
    type: Boolean,
    default: false
  }
})

const permission = {
  frozenDetail: ['wms_raw_mat_freeze:detail']
}

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
  return !props.basicClass || props.basicClass > STEEL_ENUM ? 160 : 120
})

// 是否显示物料种类全路径
const showClassifyName = computed(() => isBlank(props.columns) || props.columns.visible('classifyName'))
// 是否显示编号
const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible('serialNumber'))
// 可查看冻结信息
const frozenViewable = computed(() => props.frozenViewable && checkPermission(permission.frozenDetail))
// 列可排序
const sortable = computed(() => (props.sortable === true ? 'custom' : false))
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
  operateNumber.value = 0
  currentMaterial.value = row
  freezeDialogVisible.value = true
}

// 打开退货详情
function openMatRejectDetail(row) {
  if (!props.rejectDetailViewable) return
  currentMaterial.value = row
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

.material-classify {
  line-height: 20px;
  height: 35px;
  overflow: visible;
  // margin-top: -5px;
  display: flex;
  flex-direction: column;
  justify-content: space-around;
  align-items: center;
  .main-name {
    font-weight: 700;
  }
  .path-name {
    color: rgb(144, 147, 153);
    // display: block;
    // margin-top: 3px;
  }
}
</style>
