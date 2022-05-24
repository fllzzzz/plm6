<template>
  <common-dialog
    custom-class="class-measure-preview"
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
    <common-table :data="modifiedList" :max-height="maxHeight" empty-text="未做改动" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="first" label="一级科目">
        <template v-slot="scope">
          <span>{{ `${scope.row.fullName[0]}-${scope.row.fullSerialNumber[0]}` }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="level > 1" :show-overflow-tooltip="true" prop="second" label="二级科目">
        <template v-slot="scope">
          <span v-if="scope.row.fullName.length > 1">{{ `${scope.row.fullName[1]}-${scope.row.fullSerialNumber[1]}` }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="level > 2" :show-overflow-tooltip="true" prop="third" label="三级科目">
        <template v-slot="scope">
          <span v-if="scope.row.fullName.length > 2">{{ `${scope.row.fullName[2]}-${scope.row.fullSerialNumber[2]}` }}</span>
        </template>
      </el-table-column>
      <el-table-column label="研发费占比（%）" align="center">
        <template v-slot="scope">
          <cell-change-preview :old="scope.row.sourceRdRate" :new="scope.row.rdRate" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/config/wms/high-tech-rd-fee'
import { computed, defineEmits, defineProps, inject, ref } from 'vue'
import { isBlank } from '@data-type'
import { judgeItemFieldChange } from '@/utils'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElNotification } from 'element-plus'
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
  },
  level: {
    type: Number,
    default: 3
  }
})

const crud = inject('crud')
const sourceMap = inject('sourceMap')
const modifiedList = computed(() => props.data.filter(v => judgeItemFieldChange(v, sourceMap)))
const loading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.class-measure-preview',
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
        rdRate: v.rdRate
      }
    })
    await save(details)
    handleClose() // 关闭窗口
    crud.refresh() // 刷新页面
    emit('saveSuccess')
    ElNotification({ title: '修改成功', type: 'success' })
  } catch (error) {
    console.log('计量配置批量修改', error)
  } finally {
    loading.value = false
  }
}
</script>
