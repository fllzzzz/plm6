<template>
  <common-dialog
    custom-class="project-preparation-range-preview-dlg"
    title="变更详情"
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
      <el-table-column label="项目" align="left">
        <template #default="{ row }">
          <span v-parse-project="{ project: row }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column label="结构" align="center">
        <el-table-column label="备料范围" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview
              :old="scope.row.sourceStrucPreparationRangeType"
              :new="scope.row.strucPreparationRangeType"
              :enum="preparationRangeEnum"
            />
          </template>
        </el-table-column>
        <el-table-column label="无清单备料" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview :old="scope.row.sourceStrucWithoutList" :new="scope.row.strucWithoutList" :enum="whetherEnum" />
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column label="围护" align="center">
        <el-table-column label="备料范围" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview
              :old="scope.row.sourceEnclPreparationRangeType"
              :new="scope.row.enclPreparationRangeType"
              :enum="preparationRangeEnum"
            />
          </template>
        </el-table-column>
        <el-table-column label="无清单备料" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview :old="scope.row.sourceEnclWithoutList" :new="scope.row.enclWithoutList" :enum="whetherEnum" />
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column label="辅材" align="center">
        <el-table-column label="备料范围" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview
              :old="scope.row.sourceAuxPreparationRangeType"
              :new="scope.row.auxPreparationRangeType"
              :enum="preparationRangeEnum"
            />
          </template>
        </el-table-column>
        <el-table-column label="无清单备料" align="center" width="120">
          <template v-slot="scope">
            <cell-change-preview :old="scope.row.sourceAuxWithoutList" :new="scope.row.auxWithoutList" :enum="whetherEnum" />
          </template>
        </el-table-column>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { setProjectListForRangeInfo as save } from '@/api/plan/material-preparation/project-preparation'
import { computed, defineEmits, defineProps, inject, ref } from 'vue'
import { isBlank } from '@data-type'
import { judgeItemFieldChange } from '@/utils'
import { preparationRangeEnum } from '@enum-ms/plan'
import { whetherEnum } from '@/utils/enum/modules/common'

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

const crud = inject('crud')
const sourceMap = inject('sourceMap')
const modifiedList = computed(() => props.data.filter((v) => judgeItemFieldChange(v, sourceMap)))
const loading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.project-preparation-range-preview-dlg',
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
        strucWithoutList: v.strucWithoutList,
        enclWithoutList: v.enclWithoutList,
        auxWithoutList: v.auxWithoutList,
        strucPreparationRangeType: v.strucPreparationRangeType,
        enclPreparationRangeType: v.enclPreparationRangeType,
        auxPreparationRangeType: v.auxPreparationRangeType
      }
    })
    await save(details)
    handleClose() // 关闭窗口
    crud.refresh() // 刷新页面
    emit('saveSuccess')
    ElMessage.success('更新成功')
  } catch (error) {
    console.log('项目备料类型批量修改', error)
  } finally {
    loading.value = false
  }
}
</script>
