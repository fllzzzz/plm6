<template>
  <common-dialog
    title="零件清单明细"
    customClass="nesting-detail-dialog"
    v-model="detailDialogVisible"
    :close-on-click-modal="false"
    width="1500px"
    :showClose="true"
    :before-close="handleClose"
  >
    <common-table :data="detailData" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="project" :show-overflow-tooltip="true" label="项目" align="center">
        <template #default="{ row }">
          <span>{{ projectNameFormatter(row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" align="center" />
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" align="center" />
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" align="center" />
      <el-table-column prop="thick" :show-overflow-tooltip="true" label="厚度" align="center">
        <template #default="{ row }">
          <span>{{ row.thick }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="schThick" :show-overflow-tooltip="true" label="套料厚度" align="center">
        <template #default="{ row }">
          <span>{{ row.schThick }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" align="center">
        <template #default="{ row }">
          <span>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="schMaterial" :show-overflow-tooltip="true" label="套料材质" align="center">
        <template #default="{ row }">
          <span>{{ row.schMaterial }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" />
      <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="单重" align="center" />
      <el-table-column prop="totalNetWeight" :show-overflow-tooltip="true" label="总重" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
// import { saveNestingTask } from '@/api/mes/scheduling-manage/common'
import { getMachinePartListDetail } from '@/api/mes/scheduling-manage/machine-part'
import { projectNameFormatter } from '@/utils/project'
import { defineEmits, defineProps, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const detailData = ref([])
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailList: {
    type: Object,
    default: () => {}
  }
})

const { visible: detailDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.nesting-detail-dialog',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  detailDialogVisible
)
function showHook() {
  fetchMachinePartList()
}

async function fetchMachinePartList() {
  try {
    const data = await getMachinePartListDetail(props.detailList.id)
    detailData.value = data || []
  } catch (err) {
    console.log('获取零件清单明细失败', err)
  }
}
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
