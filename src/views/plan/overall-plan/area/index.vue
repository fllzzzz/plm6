<template>
  <div class="app-container">
    <template v-if="currentProject && currentProject.projectContentList && currentProject.projectContentList.length>0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @monomerChangeType="monomerChange"/>
      </div>
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
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="区域名称" min-width="100" />
        <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="轴线/标高" min-width="160" />
        <el-table-column v-if="columns.visible('type')" key="type" prop="type" label="制造方式" width="80">
          <template v-slot="scope">
            <el-tag effect="plain" :type="scope.row.typeTagType">{{ manufactureTypeEnum.VL[scope.row.type] }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('date')" key="endDate" prop="endDate" label="完成时间" align="center" width="180px">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.date }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" min-width="80" />
        <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="140" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
      <mForm :project-id="globalProjectId" :type-info="typeInfo" />
    </template>
    <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/area'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { manufactureTypeEnum } from '@enum-ms/plan'

const { currentProject, globalProjectId } = mapGetters(['currentProject','globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['area:get'],
  add: ['area:add'],
  edit: ['area:edit'],
  del: ['area:del']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const typeInfo = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '区域',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  }
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.area',
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

function monomerChange(val) {
  typeInfo.value = val || []
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.typeTagType = v.type === manufactureTypeEnum.HOMEMADE.V ? '' : 'warning'
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
}
</script>
