<template>
  <div v-if="!detailData.groups?.id" class="my-code">*点击左侧表格行查看详情</div>
  <div v-else>
    <div class="app-container">
      <div class="head-container" style="display: flex; justify-content: space-between">
        <!-- <el-tag>班组：{{ props.detailData?.groups?.name }}</el-tag> -->
        <div>
          <project-cascader v-model="projectId" class="filter-item" clearable style="width: 200px" />
          <monomer-select-area-select
            v-model:monomerId="monomerId"
            v-model:areaId="areaId"
            needConvert
            clearable
            areaClearable
            :project-id="projectId"
            @change="fetchGroupDetail"
          />
          <el-input
            v-model.trim="serialNumber"
            size="small"
            placeholder="编号搜索"
            style="width: 130px"
            class="filter-item"
            clearable
            @keyup.enter="fetchGroupDetail"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
            重置
          </common-button>
        </div>
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
              processId: props.processId,
              ...commonQuery,
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
        :max-height="maxHeight + 180"
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
        <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="scheduleQuantity" key="scheduleQuantity" label="排产数" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="scheduleNetWeight"
          key="scheduleNetWeight"
          label="排产量（kg）"
          align="center"
        />
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" key="completeQuantity" label="完成数" align="center">
          <template #default="{ row }">
            <span :class="row.scheduleQuantity === row.completeQuantity ? '' : 'tc-danger'">{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeNetWeight" key="completeNetWeight" label="完成量（kg）" align="center">
          <template #default="{ row }">
            <span :class="row.scheduleQuantity === row.completeQuantity ? '' : 'tc-danger'">{{ row.completeNetWeight }}</span>
          </template>
        </el-table-column>
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
import { ref, defineProps, watch, inject, computed } from 'vue'
import { projectNameFormatter } from '@/utils/project'
// import { mapGetters } from '@/store/lib'
import projectCascader from '@comp-base/project-cascader'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'

// const { globalProjectId } = mapGetters(['globalProjectId'])
const props = defineProps({
  detailData: {
    type: Object,
    default: () => {}
  },
  workshopId: {
    type: Number
  },
  processId: {
    type: Number
  }
})

const tableRef = ref()
const list = ref([])
const projectId = ref()
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

const permission = inject('permission')
watch(
  () => props.detailData?.groups?.id,
  (value) => {
    if (value) {
      fetchGroupDetail()
    }
  }, { immediate: true, deep: true }
)
// watch(
//   () => globalProjectId.value,
//   (val) => {
//     if (val) {
//       projectId.value = globalProjectId.value
//       fetchGroupDetail()
//     }
//   },
//   { immediate: true, deep: true }
// )
const commonQuery = computed(() => {
  return {
    projectId: projectId.value,
    monomerId: monomerId.value,
    areaId: areaId.value,
    serialNumber: serialNumber.value
  }
})
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchGroupDetail })

async function fetchGroupDetail() {
  try {
    const { content = [], totalElements } = await getGroupDetail({
      groupId: props.detailData.groups?.id,
      productionLineId: props.detailData.productionLine?.id,
      taskTypeEnum: props.detailData.taskTypeEnum,
      teamId: props.detailData.team?.id,
      workshopId: props.workshopId,
      processId: props.processId,
      ...commonQuery.value,
      ...queryPage
    })
    list.value = content || []
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取班组详情信息失败', error)
  }
}

function searchQuery() {
  fetchGroupDetail()
}

function resetQuery() {
  projectId.value = undefined
  monomerId.value = undefined
  areaId.value = undefined
  serialNumber.value = undefined
  fetchGroupDetail()
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
