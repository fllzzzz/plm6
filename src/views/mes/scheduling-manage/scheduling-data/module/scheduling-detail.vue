<template>
  <common-drawer
    ref="drawerRef"
    custom-class="scheduling-detail-drawer"
    :title="`排产及生产明细`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="90%"
  >
    <template #titleAfter>
      <project-cascader v-model="projectId" class="filter-item" clearable style="width: 270px" @change="fetchDetail" />
      <monomer-select-area-select
        v-model:monomerId="monomerId"
        v-model:areaId="areaId"
        needConvert
        clearable
        areaClearable
        :project-id="projectId"
        style="width: 150px"
        @change="fetchDetail"
      />
      <el-input
        v-model.trim="serialNumber"
        placeholder="编号搜索"
        style="width: 150px"
        class="filter-item"
        clearable
        @keyup.enter="fetchDetail"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <print-table
        v-permission="permission.print"
        api-key="mesSchedulingDataList"
        :params="{ dateTime: props.dateTime, workshopId: props.workshopId, ...commonQuery }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :show-empty-symbol="false" :max-height="maxHeight" :data="list" return-source-data style="width: 100%">
        <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="180">
          <template v-slot="scope">
            <span>{{ scope.row.project ? projectNameFormatter(scope.row.project) : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" key="monomer.name" label="单体" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="area.name" key="area.name" label="区域" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="specification"
          key="specification"
          label="规格"
          min-width="120"
          align="center"
        />
        <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="schedulingQuantity" key="schedulingQuantity" label="排产数" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="schedulingTotalNetWeight"
          key="schedulingTotalNetWeight"
          label="排产量（kg）"
          align="center"
        />
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" key="completeQuantity" label="完成数" align="center">
          <template #default="{ row }">
            <span :class="row.schedulingQuantity === row.completeQuantity ? 'tc-success' : 'tc-danger'">{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          :show-overflow-tooltip="true"
          prop="completeTotalNetWeight"
          key="completeTotalNetWeight"
          label="完成量（kg）"
          align="center"
        >
          <template #default="{ row }">
            <span :class="row.schedulingQuantity === row.completeQuantity ? 'tc-success' : 'tc-danger'">{{
              row.completeTotalNetWeight
            }}</span>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { getScheduleDetail } from '@/api/mes/scheduling-manage/scheduling-data.js'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { projectNameFormatter } from '@/utils/project'
import projectCascader from '@comp-base/project-cascader'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { schedulingDataPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['update:visible'])
const list = ref([])
const projectId = ref()
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  dateTime: {
    type: String,
  },
  workshopId: {
    type: Number,
  },
})

const commonQuery = computed(() => {
  return {
    projectId: projectId.value,
    monomerId: monomerId.value,
    areaId: areaId.value,
    serialNumber: serialNumber.value,
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchDetail })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.scheduling-detail-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true,
  },
  drawerVisible
)

async function fetchDetail() {
  let _list = []
  try {
    const { content = [], totalElements } = await getScheduleDetail({
      dateTime: props.dateTime,
      workshopId: props.workshopId,
      ...commonQuery.value,
      ...queryPage,
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取排产数据详情失败', e)
  } finally {
    list.value = _list
  }
}

function searchQuery() {
  fetchDetail()
}

function resetQuery() {
  projectId.value = undefined
  monomerId.value = undefined
  areaId.value = undefined
  serialNumber.value = undefined
  fetchDetail()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

