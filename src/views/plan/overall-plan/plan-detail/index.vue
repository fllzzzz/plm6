<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--工具栏-->
      <!-- <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div> -->
      <!--表格渲染-->
      <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="日期" min-width="100" />
      <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="提交人" min-width="180" />
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="单体名称" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column> <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="工作内容" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="区域" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="计划完成（天）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="实际完成（天）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="完成度" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="单位" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="完成量" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="操作人" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.className }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <!-- <el-table-column
        v-if="checkPermission([...permission.edit])"
        label="状态"
        width="160px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
        </template>
      </el-table-column> -->
    </common-table>
      <!--分页组件-->
      <pagination />
    </template>
    <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-make'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { dateDifference } from '@/utils/date'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['plan:get'],
  edit: ['plan:edit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '区域计划',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-make',
  paginate: true,
  extraHeight: 157
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.typeTagType = v.type === manufactureTypeEnum.HOMEMADE.V ? '' : 'warning'
    if (v.startDate && v.endDate) {
      v.dateDifference = dateDifference(v.startDate, v.endDate) + '天'
    } else {
      v.dateDifference = ''
    }
    v.sourceRemark = v.remark
    v.sourceStartDate = v.startDate
    v.sourceEndDate = v.endDate
    v.startDate = v.startDate ? v.startDate + '' : undefined
    v.endDate = v.endDate ? v.endDate + '' : undefined
    v.modifying = false
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
}
</script>
