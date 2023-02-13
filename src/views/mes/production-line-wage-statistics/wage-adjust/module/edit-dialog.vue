<template>
  <common-dialog title="批量调整工价" v-model="dialogVisible" width="1500px" top="5vh" :before-close="handleClose">
    <template #titleRight>
      <common-button
        v-permission="permission.edit"
        type="primary"
        size="mini"
        :disabled="!modifiedData.length"
        @click="previewVisible = true"
        >预览并保存</common-button
      >
    </template>
    <common-table
      ref="tableRef"
      return-source-data
      :show-empty-symbol="false"
      :data-format="dataFormat"
      :data="list"
      :max-height="maxHeight - 100"
      row-key="rowId"
      style="width: 100%"
      :cell-class-name="changedCellMask"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="130px" />
      <el-table-column prop="quantity" label="数量" align="center" show-overflow-tooltip min-width="70px" />
      <el-table-column prop="netWeight" label="重量(kg)" align="center" show-overflow-tooltip min-width="90px" />
      <el-table-column v-if="props.processInfo?.type !== processCategoryEnum.PAINT.V" prop="wageQuotaTypeStr" align="center" show-overflow-tooltip label="核算单位" width="70px" />
      <template v-if="showMoreColumn">
        <!-- <el-table-column align="center" prop="wage" label="定额单价" width="150">
          <template #default="{ row }">
            <common-input-number
              v-model="row.wage"
              :disabled="!!(row.wageQuotaType & wageQuotaTypeEnum.AREA.V)"
              placeholder="请输入单价"
              :precision="2"
              :controls="false"
              style="width: 100%"
            />
          </template>
        </el-table-column> -->
        <el-table-column align="center" prop="primer" label="底漆" width="150">
          <el-table-column prop="primerWageQuotaType" align="center" show-overflow-tooltip label="核算单位" width="70px">
            <template #default="{ row }">
              <span>{{ wageQuotaTypeEnum.V[row.primerWageQuotaType]?.C_UNIT }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="primerWage" label="定额单价" width="150">
            <template #default="{ row }">
              <common-input-number v-model="row.primerWage" placeholder="请输入单价" :precision="2" :controls="false" style="width: 100%" />
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column align="center" prop="intermediatePaint" label="中间漆" width="150">
          <el-table-column prop="intermediatePaintWageQuotaType" align="center" show-overflow-tooltip label="核算单位" width="70px">
            <template #default="{ row }">
              <span>{{ wageQuotaTypeEnum.V[row.intermediatePaintWageQuotaType]?.C_UNIT }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="intermediatePaintWage" label="定额单价" width="150">
            <template #default="{ row }">
              <common-input-number
                v-model="row.intermediatePaintWage"
                placeholder="请输入单价"
                :precision="2"
                :controls="false"
                style="width: 100%"
              />
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column align="center" prop="topcoat" label="面漆" width="150">
          <el-table-column prop="topcoatWageQuotaType" align="center" show-overflow-tooltip label="核算单位" width="70px">
            <template #default="{ row }">
              <span>{{ wageQuotaTypeEnum.V[row.topcoatWageQuotaType]?.C_UNIT }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="topcoatWage" label="定额单价" width="150">
            <template #default="{ row }">
              <common-input-number
                v-model="row.topcoatWage"
                placeholder="请输入单价"
                :precision="2"
                :controls="false"
                style="width: 100%"
              />
            </template>
          </el-table-column>
        </el-table-column>
      </template>
      <template v-else>
        <!-- <el-table-column align="center" prop="sourceWage" label="定额单价"> </el-table-column> -->
        <el-table-column align="center" prop="wage" label="定额单价" width="200">
          <template #default="{ row }">
            <common-input-number
              v-model="row.wage"
              placeholder="请输入单价"
              :precision="2"
              :controls="false"
              :min="0"
              style="width: 100%"
            />
          </template>
        </el-table-column>
      </template>
    </common-table>
    <edit-preview
      v-model:visible="previewVisible"
      :modified-data="modifiedData"
      :showMoreColumn="showMoreColumn"
      @success="handleEditSuccess"
    />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, ref, computed, inject } from 'vue'

import { wageQuotaTypeEnum, processCategoryEnum } from '@enum-ms/mes'

import useTableChange from '@compos/form/use-table-change'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import editPreview from './edit-preview.vue'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  selections: {
    type: Array,
    default: () => []
  },
  processInfo: {
    type: Object,
    default: () => {}
  }
})

const sourceMap = new Map([
  ['wage', 'sourceWage'],
  ['intermediatePaintWage', 'sourceIntermediatePaintWage'],
  ['primerWage', 'sourcePrimerWage'],
  ['sourceTopcoatWage', 'topcoatWage']
])

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook, closeHook: beforeClose })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

const permission = inject('permission')
const dataFormat = [['wageQuotaTypeStr', ['parse-enum', wageQuotaTypeEnum, { f: 'meteUnit' }], { source: 'wageQuotaType' }]]

const list = ref([])
const previewVisible = ref(false)
const hasEdit = ref(false)
const hasAreaQuota = ref(false)

// 油漆类 且计价方式为面积 需要显示底漆、中间漆、面漆的单价
const showMoreColumn = computed(() => {
  return !!(props.processInfo && props.processInfo?.type & processCategoryEnum.PAINT.V && true)
})

function showHook() {
  list.value = props.selections.map((v) => {
    if (v.wageQuotaType & wageQuotaTypeEnum.AREA.V) {
      hasAreaQuota.value = true
    }
    return v
  })
}

const modifiedData = computed(() => {
  return list.value.filter(
    (v) =>
      v.sourceWage !== v.wage ||
      v.sourceIntermediatePaintWage !== v.intermediatePaintWage ||
      v.sourcePrimerWage !== v.primerWage ||
      v.sourceTopcoatWage !== v.topcoatWage
  )
})

function handleEditSuccess() {
  hasEdit.value = true
  handleClose()
}

function beforeClose() {
  list.value = []
  hasAreaQuota.value = false
  if (hasEdit.value) {
    emit('refresh')
  }
}
</script>
