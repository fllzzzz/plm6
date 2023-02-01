<template>
  <common-dialog title="工价修改预览" v-model="dialogVisible" top="5vh" append-to-body :before-close="handleClose" width="1200px">
    <template #titleRight>
      <common-button :loading="loading" size="mini" :disabled="!modifiedData || modifiedData.length == 0" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="modifiedData" :max-height="maxHeight" empty-text="未做改动" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="110px" />
      <el-table-column prop="wageQuotaTypeStr" align="center" show-overflow-tooltip label="核算单位" width="70px" />
      <el-table-column align="center" prop="wage" label="定额单价" min-width="100px">
        <template #default="{ row }">
          <cell-change-preview :old="row.sourceWage" :new="row.wage" />
        </template>
      </el-table-column>
      <template v-if="showMoreColumn">
        <el-table-column align="center" prop="primerWage" label="底漆" min-width="100px">
          <template #default="{ row }">
            <cell-change-preview :old="row.sourcePrimerWage" :new="row.primerWage" />
          </template>
        </el-table-column>
        <el-table-column align="center" prop="intermediatePaintWage" label="中间漆" min-width="100px">
          <template #default="{ row }">
            <cell-change-preview :old="row.sourceIntermediatePaintWage" :new="row.intermediatePaintWage" />
          </template>
        </el-table-column>
        <el-table-column align="center" prop="topcoatWage" label="面漆" min-width="100px">
          <template #default="{ row }">
            <cell-change-preview :old="row.sourceTopcoatWage" :new="row.topcoatWage" />
          </template>
        </el-table-column>
      </template>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { edit } from '@/api/mes/production-line-wage-statistics/wage-adjust'

import { defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cellChangePreview from '@comp-common/cell-change-preview'

const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  modifiedData: {
    type: Array,
    default: () => []
  },
  showMoreColumn: {
    type: Boolean,
    default: false
  }
})

const loading = ref(false)

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false,
    extraHeight: 150
  },
  dialogVisible
)

async function submit() {
  try {
    loading.value = true
    const _list = props.modifiedData.map((v) => {
      return {
        id: v.id,
        intermediatePaintWage: v.intermediatePaintWage,
        primerWage: v.primerWage,
        processId: v.processId,
        taskTypeEnum: v.taskTypeEnum,
        topcoatWage: v.topcoatWage,
        wage: v.wage,
        wageQuotaType: v.wageQuotaType
      }
    })
    await edit(_list)
    ElNotification({ title: '工价修改成功', type: 'success' })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('操作失败', error)
  } finally {
    loading.value = false
  }
}
</script>
