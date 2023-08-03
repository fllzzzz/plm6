<template>
  <common-dialog
    custom-class="manual-filling-preview-dlg"
    title="手工填报修改预览"
    append-to-body
    v-model="dialogVisible"
    width="80vw"
    :before-close="handleClose"
    :top="'5vh'"
  >
    <template #titleRight>
      <common-button :loading="loading" :disabled="isBlank(modifiedList)" type="primary" size="mini" @click="submit">保 存</common-button>
    </template>
    <common-table :data="modifiedList" :max-height="maxHeight" return-source-data empty-text="未做改动" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" align="center" min-width="120px" />
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" align="center" min-width="120px" />
      <el-table-column prop="artifactClass.name" label="构件类型" align="center" min-width="100px" />
      <el-table-column label="油漆面积(㎡)" align="center" min-width="100px">
        <template v-slot="{ row }">
          <cell-change-preview :old="row.sourcePaintArea" :new="row.paintArea" />
        </template>
      </el-table-column>
      <el-table-column label="底漆" align="center">
        <el-table-column label="品牌" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourcePrimerBrand" :new="row.primerBrand" />
          </template>
        </el-table-column>
        <el-table-column label="用量(kg)" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourcePrimerDosage" :new="row.primerDosage" />
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column label="中间漆" align="center">
        <el-table-column label="品牌" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourceIntermediatePaintBrand" :new="row.intermediatePaintBrand" />
          </template>
        </el-table-column>
        <el-table-column label="用量(kg)" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourceIntermediatePaintDosage" :new="row.intermediatePaintDosage" />
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column label="面漆" align="center">
        <el-table-column label="品牌" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourceTopcoatBrand" :new="row.topcoatBrand" />
          </template>
        </el-table-column>
        <el-table-column label="用量(kg)" align="center" min-width="100px">
          <template v-slot="{ row }">
            <cell-change-preview :old="row.sourceTopcoatDosage" :new="row.topcoatDosage" />
          </template>
        </el-table-column>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { manualEdit } from '@/api/mes/production-manage/dashboard/painting'
import { computed, defineEmits, defineProps, inject, ref } from 'vue'

import { isBlank } from '@data-type'
import { judgeItemFieldChange } from '@/utils'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'
import cellChangePreview from '@comp-common/cell-change-preview'

const emit = defineEmits(['saveSuccess', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  data: {
    type: Array,
    default: () => []
  }
})

const sourceMap = inject('sourceMap')
const modifiedList = computed(() => props.data.filter((v) => judgeItemFieldChange(v, sourceMap)))
const loading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.manual-filling-preview-dlg',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

async function submit() {
  try {
    loading.value = true
    await manualEdit(modifiedList.value)
    handleClose() // 关闭窗口
    emit('saveSuccess')
    ElMessage.success('更新成功')
  } catch (error) {
    console.log('保存手工填报', error)
  } finally {
    loading.value = false
  }
}
</script>
