<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="80%"
    :title="`项目详情`"
    :before-close="handleClose"
    :show-close="true"
    :close-on-click-modal="false"
    top="10vh"
  >
    <template #content>
      <div class="head-container" style="display: flex; justify-content: space-between">
        <div>
          <monomer-select-area-select
            v-model:monomerId="monomerId"
            v-model:areaId="areaId"
            needConvert
            clearable
            :default="false"
            :project-id="props.detailData.project.id"
            @change="showProjectChange"
          />
          <el-input
            v-model.trim="serialNumber"
            size="small"
            placeholder="输入编号搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="showProjectChange"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
            重置
          </common-button>
        </div>
        <div>
          <print-table
          v-permission="permission.print"
            api-key="mesMonthlyTaskList"
            :params="{
              projectId: props.detailData.project.id,
              monomerId: monomerId,
              areaId: areaId,
              serialNumber: serialNumber,
            }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </div>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        :data="projectDetailData"
        return-source-data
        :max-height="maxHeight + 30"
        highlight-current-row
        :showEmptySymbol="false"
        style="width: 100%"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="150">
          <template v-slot="scope">
            <span>{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" align="center" width="160px"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" width="150px"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单净重" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="grossWeight" label="单毛重" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="排产数" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" label="完成数" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeNetWeight" label="完成量" align="center"></el-table-column>
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
import { projectDetail } from '@/api/mes/task-tracking/monthly-task-tracking.js'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { defineProps, defineEmits, ref } from 'vue'
import { mesMonthlyTaskTrackingPM as permission } from '@/page-permission/mes'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { projectNameFormatter } from '@/utils/project'

const emit = defineEmits(['update:visible'])
const projectDetailData = ref([])
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showProjectDetail })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: showProjectDetail })

async function showProjectDetail() {
  let _list = []
  try {
    const { content = [], totalElements } = await projectDetail({
      projectId: props.detailData.project.id,
      monomerId: monomerId.value,
      areaId: areaId.value,
      serialNumber: serialNumber.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取项目详情失败', e)
  } finally {
    projectDetailData.value = _list
  }
}

// 搜索
function searchQuery() {
  showProjectDetail()
}
// 重置
function resetQuery() {
  monomerId.value = undefined
  areaId.value = undefined
  serialNumber.value = undefined
  showProjectDetail()
}

function showProjectChange() {
  showProjectDetail()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

