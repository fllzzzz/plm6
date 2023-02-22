<template>
  <div v-if="!detailData.groups?.id" class="my-code">*点击左侧表格行查看详情</div>
  <div v-else>
    <div class="app-container">
      <div class="head-container" style="display: flex; justify-content: space-between">
        <el-tag>班组：{{ props.detailData?.groups?.name }}</el-tag>
        <div style="width: 300px">
          <print-table
            :api-key="apiKey"
            :params="{ projectId: props.detailData.projectId }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </div>
      </div>
      <common-table ref="tableRef" :data="list" :empty-text="'暂无数据'" :max-height="maxHeight" row-key="id" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column prop="project" key="project" label="项目" align="center">
          <template #default="{ row }">
            <span>{{ projectNameFormatter(row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="monomer.name" key="monomer.name" label="单体" align="center" />
        <el-table-column prop="area.name" key="area.name" label="区域" align="center" />
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column prop="specification" key="specification" label="规格" align="center" />
        <el-table-column prop="quantity" key="quantity" label="数量" align="center" />
        <el-table-column prop="netWeight" key="netWeight" label="重量（kg）" align="center" />
      </common-table>
    </div>
  </div>
</template>
<script setup>
import { getGroupDetail } from '@/api/mes/production-monitoring-kanban/kanban.js'
import { ref, defineProps, watch } from 'vue'
import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  detailData: {
    type: Object,
    default: () => {}
  },
  workshopId: {
    type: Number
  }
})

const tableRef = ref()
const list = ref([])

const { maxHeight } = useMaxHeight({
  paginate: true
})

watch(
  () => props.detailData?.groups?.id,
  (value) => {
    fetchGroupDetail()
  }
)

async function fetchGroupDetail() {
  try {
    const { content } = await getGroupDetail({
      groupId: props.detailData.groups?.id,
      productionLineId: props.detailData.productionLine?.id,
      taskTypeEnum: props.detailData.taskTypeEnum,
      teamId: props.detailData.team?.id,
      workshopId: props.workshopId
    })
    list.value = content || []
  } catch (error) {
    console.log('获取班组详情信息失败', error)
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
