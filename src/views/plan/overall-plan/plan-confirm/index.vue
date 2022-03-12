<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <mHeader />
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="[{id:1}]"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%;margin-top:20px;"
      >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="日期" width="150" />
      <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="提交人" width="120" />
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
      <el-table-column v-if="columns.visible('className')" key="sort" prop="sort" label="单位" align="center" width="60">
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
      <el-table-column
        label="操作"
        width="160px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <el-popconfirm title="一旦确认，将不能修改，确认完成?" @confirm="confirmEvent(scope.row.name)">
            <template #reference>
              <common-button type="primary" size="small">确定完成</common-button>
            </template>
          </el-popconfirm>
        </template>
      </el-table-column>
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
// import checkPermission from '@/utils/system/check-permission'
import mHeader from './module/header'
import { planConfirmListPM as permission } from '@/page-permission/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '工作确认',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-confirm',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

async function confirmEvent(name) {
  try {
    // await editStatus(data.id, val)
    // crud.notify(`“${name}” 确认成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    // crud.refresh()
  } catch (e) {
    console.log('计划确认', e)
  }
}

// CRUD.HOOK.handleRefresh = (crud, data) => {
//   data.data.content = data.data.content.map(v => {
//     v.typeTagType = v.type === manufactureTypeEnum.HOMEMADE.V ? '' : 'warning'
//     if (v.startDate && v.endDate) {
//       v.dateDifference = dateDifferenceReduce(v.startDate, v.endDate) + '天'
//     } else {
//       v.dateDifference = ''
//     }
//     v.sourceRemark = v.remark
//     v.sourceStartDate = v.startDate
//     v.sourceEndDate = v.endDate
//     v.startDate = v.startDate ? v.startDate + '' : undefined
//     v.endDate = v.endDate ? v.endDate + '' : undefined
//     v.modifying = false
//     return v
//   })
// }

// CRUD.HOOK.beforeSubmit = () => {
//   crud.form.projectId = globalProjectId
//   return !!crud.form.projectId
// }
</script>

