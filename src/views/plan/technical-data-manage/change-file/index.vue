<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :project-id="globalProjectId"/>
    </div>
    <!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    @row-dblclick="dbclick"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="文件" min-width="160px" />
    <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" width="350px">
      <!-- <template slot="header">
        <el-tooltip
          class="item"
          effect="light"
          :content="`双击备注可修改`"
          placement="top"
        >
          <div style="display:inline-block;">
            <span>备注</span>
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template> -->
      <!-- <template v-slot="scope">
        <span v-if="!scope.row.edit">{{ scope.row.remark }}</span>
        <span v-else>
          <el-input
            v-model="scope.row.remark"
            type="textarea"
            :rows="1"
            size="mini"
            style="width:180px;"
          />
          <common-button size="mini" type="primary" :loading="scope.row.editLoading" @click="saveIt(scope.row)">保存</common-button>
          <common-button size="mini" type="info" @click="cancelIt(scope.row)">取消</common-button>
        </span>
      </template> -->
    </el-table-column>
    <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="导入人" width="200px" />
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="160px">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'" >{{ scope.row.createTime }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.download,...permission.del])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <udOperation
          :data="scope.row"
          :show-edit="false"
        />
        <!-- 下载 -->
        <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/other'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'

const { globalProjectId } = mapGetters(['globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['changeFile:get'],
  download: ['changeFile:download'],
  del: ['changeFile:del'],
  import: ['changeFile:import']
}

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更文件',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId', 'type'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.changeFile',
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
  { immediate: true, deep: true }
)

function dbclick(row, column, event) {
  if (column.property === 'remark' && !row.edit) {
    row.edit = true
  }
}

// function cancelIt(row) {
//   row.remark = row.originalRemark
//   row.edit = false
// }

// async function saveIt(row) {
//   try {
//     row.editLoading = true
//     await edit(row.id, {
//       remark: row.remark
//     })
//     this.$notify({ title: '修改成功', type: 'success', duration: 2500 })
//   } catch (error) {
//     console.log('编辑备注', error)
//   } finally {
//     row.edit = false
//     row.editLoading = false
//   }
// }

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.edit = false
    v.originalRemark = v.remark
    v.editLoading = false
    return v
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
