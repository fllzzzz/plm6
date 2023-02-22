<template>
  <common-dialog
    title="生产监控看板详情"
    customClass="production-detail-dialog"
    v-model="detailDialogVisible"
    :close-on-click-modal="false"
    width="1500px"
    :showClose="true"
    :before-close="handleClose"
  >
    <template #titleAfter>
      <el-tag size="small">项目：{{ detailList.project?.contractNo }}-{{ detailList.project?.name }}</el-tag>
      <el-tag size="small" style="margin-left: 8px">排产总量（件/kg）：{{ detailList.taskQuantity }}/{{ detailList.taskNetWeight }}</el-tag>
    </template>
    <div class="head-dialog" style="width: 270px; margin-bottom: 8px; float: right">
      <print-table
        api-key="mesFactoryWorkshopReport"
        :params="{
          projectId: props.detailList?.project?.id,
          monomerId: props.detailList?.monomer?.id,
          areaId: props.detailList?.area?.id,
          workshopId: props.workshopId,
        }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </div>
    <common-table :data="detailData" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" align="center" />
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" align="center" />
      <el-table-column prop="name" :show-overflow-tooltip="true" label="名称" align="center" />
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" align="center" />
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" align="center" />
      <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" align="center" />
      <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" align="center">
        <template #default="{ row }">
          <span>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="排产量" align="center" />
      <el-table-column prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" align="center" />
      <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="实际完成数" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { getDetail } from '@/api/mes/production-monitoring-kanban/kanban.js'
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
  },
  workshopId: {
    type: Number
  }
})

const { visible: detailDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchMachinePartList })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.production-detail-dialog',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  detailDialogVisible
)

async function fetchMachinePartList() {
  try {
    const { content } = await getDetail({
      projectId: props.detailList?.project?.id,
      monomerId: props.detailList?.monomer?.id,
      areaId: props.detailList?.area?.id,
      workshopId: props.workshopId
    })
    detailData.value = content || []
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
