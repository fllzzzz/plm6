<template>
<el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">分包类别</span>
        <common-button size="mini" type="primary" style="float: right" @click="crud.toAdd" v-permission="permission.add">
          新增
        </common-button>
      </div>
    </template>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="分包类别" min-width="150">
        <template v-slot="scope">
          <el-input v-model="scope.row.name" type="text" placeholder="分包类别" style="width: 260px" maxlength="50" v-if="scope.row.edit"/>
          <div v-else>{{ scope.row.name }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="150px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <template v-if="scope.row.edit">
            <common-button type="info" size="mini" @click="scope.row.name=scope.row.sourceRow.name;scope.row.edit=false;">取消</common-button>
            <common-button type="primary" size="mini" @click="rowSubmit(scope.row)">保存</common-button>
          </template>
          <template v-else>
            <common-button v-if="checkPermission(permission.edit) && scope.row.name!=='劳务分包'" size="mini" icon="el-icon-edit" type="primary" @click="scope.row.edit=true"/>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="确定删除吗?"
              @confirm="deleteRow(scope.row)"
              v-if="checkPermission(permission.del) && scope.row.name!=='劳务分包'"
            >
              <template #reference>
                <common-button size="small" class="el-icon-delete" type="danger"/>
              </template>
            </el-popconfirm>
          </template>
        </template>
      </el-table-column>
    </common-table>
  </el-card>
  <div class="app-container">
  <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/project-config/subcontract-config'
import { ref } from 'vue'

import { subcontractConfigPM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'
import { ElMessage } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mForm from './module/form'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '分包类别',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.subcontractConfig',
  paginate: true,
  extraHeight: 200
})

async function rowSubmit(row) {
  if (!row.name) {
    ElMessage.error('请填写分包类别')
    return
  }
  try {
    await crudApi.edit(row)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.edit = false
  } catch (e) {
    console.log('分包类别', e)
  }
}

async function deleteRow(row) {
  try {
    await crudApi.del([row.id])
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('删除', e)
  }
}

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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
