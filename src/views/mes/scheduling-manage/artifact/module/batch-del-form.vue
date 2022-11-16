<template>
  <common-dialog
    title="批量删除任务"
    customClass="artifact-scheduling-batch-del"
    v-model="dialogVisible"
    top="10vh"
    width="1100px"
    :before-close="handleClose"
  >
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        <span>原产线：</span>
        <span>{{ originLabel }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="confirmIt">确 认</common-button>
    </template>
    <common-table :data="list" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.monomer?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.area?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
      <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
      <el-table-column prop="delQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center">
        <template #default="{ row }">
          <common-input-number
            v-model="row.delQuantity"
            :step="1"
            :min="1"
            :max="row.schedulingQuantity"
            :precision="0"
            size="small"
            controls-position="right"
            style="width: 100%"
          />
        </template>
      </el-table-column>
      <el-table-column prop="schedulingTotalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="90" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { delRecord } from '@/api/mes/scheduling-manage/artifact'
import { computed, defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'del-success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  selections: {
    type: Array,
    default: () => []
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.artifact-scheduling-batch-del',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const originLabel = computed(() => {
  const v = props.selections?.length ? props.selections[0] : {}
  return `${v.workshop?.name}>${v.productionLine?.name}>${v.groups?.name}`
})

const submitLoading = ref(false)
const list = ref([])

function showHook() {
  list.value = props.selections.map((v) => {
    return {
      ...v,
      delQuantity: v.schedulingQuantity
    }
  })
}

async function confirmIt(id, schedulingQuantity) {
  try {
    submitLoading.value = true
    const _list = list.value.map((v) => {
      return {
        id: v.id,
        quantity: v.delQuantity
      }
    })
    await delRecord(_list)
    ElNotification({
      title: '批量删除任务成功',
      type: 'success',
      duration: 2500
    })
    emit('del-success')
    handleClose()
  } catch (error) {
    console.log('删除任务失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
