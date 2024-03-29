<template>
  <div v-if="!detailData.groups?.id" class="my-code">*点击左侧表格行查看详情</div>
  <div v-else>
    <div class="app-container">
      <div class="head-container" style="display: flex; justify-content: space-between">
        <el-tag>班组：{{ props.detailData?.groups?.name }}</el-tag>
        <div style="width: 300px">
          <print-table
            v-permission="permission.print"
            api-key="mesProductionKanbanGroupList"
            :params="{
              groupId: props.detailData.groups?.id,
              productionLineId: props.detailData.productionLine?.id,
              taskTypeEnum: props.detailData.taskTypeEnum,
              teamId: props.detailData.team?.id,
              workshopId: props.workshopId,
              areaId: props.areaId
            }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </div>
      </div>
      <common-table
        ref="tableRef"
        :data="list"
        :empty-text="'暂无数据'"
        :show-empty-symbol="false"
        :max-height="maxHeight + 120"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="project" key="project" label="项目" align="center">
          <template #default="{ row }">
            <span>{{ row.project ? projectNameFormatter(row.project) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" key="monomer.name" label="单体" align="center">
          <template #default="{ row }">
            <span>{{ row.monomer ? row.monomer?.name : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="area.name" key="area.name" label="区域" align="center">
          <template #default="{ row }">
            <span>{{ row.area ? row.area?.name : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" key="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" key="netWeight" label="重量（kg）" align="center" />
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </div>
  </div>
</template>
<script setup>
import { getGroupDetail } from '@/api/mes/production-monitoring-kanban/kanban.js'
import { ref, defineProps, watch, inject } from 'vue'
import { projectNameFormatter } from '@/utils/project'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  detailData: {
    type: Object,
    default: () => {}
  },
  workshopId: {
    type: Number
  },
  areaId: {
    type: Number
  }
})

const tableRef = ref()
const list = ref([])

const { maxHeight } = useMaxHeight({
  paginate: true
})

const permission = inject('permission')
watch(
  () => props.detailData?.groups?.id,
  (value) => {
    if (value) {
      fetchGroupDetail()
    }
  }
)
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchGroupDetail })

async function fetchGroupDetail() {
  try {
    const { content = [], totalElements } = await getGroupDetail({
      groupId: props.detailData.groups?.id,
      productionLineId: props.detailData.productionLine?.id,
      taskTypeEnum: props.detailData.taskTypeEnum,
      teamId: props.detailData.team?.id,
      workshopId: props.workshopId,
      areaId: props.areaId,
      ...queryPage
    })
    list.value = content || []
    setTotalPage(totalElements)
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
