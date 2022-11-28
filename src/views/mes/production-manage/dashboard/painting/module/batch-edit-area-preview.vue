<template>
  <common-dialog
    custom-class="batch-edit-area-preview-dlg"
    title="面积修改预览"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
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
      <el-table-column prop="serialNumber" label="编号" align="center" min-width="80px" />
      <el-table-column prop="quantity" label="编号" align="center" min-width="80px" />
      <el-table-column label="面积(㎡)" align="center">
        <template v-slot="scope">
          <cell-change-preview :old="scope.row.sourceSurfaceArea" :new="scope.row.surfaceArea" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { editArea } from '@/api/mes/production-manage/dashboard/painting'
import { computed, defineEmits, defineProps, inject, ref } from 'vue'
import { isBlank } from '@data-type'
import { judgeItemFieldChange } from '@/utils'
import { convertUnits } from '@/utils/convert/unit'

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
    mainBox: '.batch-edit-area-preview-dlg',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

async function submit() {
  try {
    loading.value = true
    const details = modifiedList.value.map((v) => {
      return {
        id: v.id,
        surfaceArea: convertUnits(v.surfaceArea, '㎡', 'mm²')
      }
    })
    await editArea(details)
    handleClose() // 关闭窗口
    emit('saveSuccess')
    ElMessage.success('更新成功')
  } catch (error) {
    console.log('面积批量修改', error)
  } finally {
    loading.value = false
  }
}
</script>
