<template>
  <common-dialog title="班组工价修改预览" v-model="dialogVisible" top="5vh" append-to-body :before-close="handleClose" width="50%">
    <template #titleRight>
      <common-button :loading="loading" size="mini" :disabled="!modifiedData || modifiedData.length == 0" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="modifiedData" :max-height="maxHeight" empty-text="未做改动" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns showProductionLine showTeam />
      <el-table-column align="center" prop="price" label="单价">
        <template #default="{ row }">
          <cell-change-preview :old="row.wages" :new="row.price" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { edit } from '@/api/mes/team-report/wages-adjust/index'

import { defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
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
  data: {
    type: Object,
    default: () => {}
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
    const _list = props.modifiedData
      .filter((v) => v.price)
      .map((v) => {
        return {
          teamId: v.id,
          wages: v.price
        }
      })
    await edit({
      details: _list,
      ...props.data
    })
    ElNotification({ title: '班组工价修改成功', type: 'success' })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('操作失败', error)
  } finally {
    loading.value = false
  }
}
</script>
