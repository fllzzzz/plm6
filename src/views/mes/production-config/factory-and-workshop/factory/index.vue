<template>
  <el-card id="factory-container">
    <template v-slot:header>
      <span style="line-height: 1.7">{{ crud.title }}列表</span>
      <!-- 新增 -->
      <common-button
        v-permission="permission.add"
        class="filter-item"
        style="float: right; padding: 6px 10px"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click="crud.toAdd"
      >
        新增
      </common-button>
    </template>
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      highlight-current-row
      :max-height="maxHeight"
      :empty-text="crud.emptyText"
      style="width: 100%"
      @current-change="handleCurrentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="工厂名称" min-width="140px" />
      <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="工厂简称"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('tagColor')"
        key="tagColor"
        prop="tagColor"
        :show-overflow-tooltip="true"
        label="标签颜色"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span class="color-card" :style="{ 'background-color': scope.row.tagColor || TAG_FACTORY_DEF_COLOR }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('boolEnabledEnum')"
        key="boolEnabledEnum"
        prop="boolEnabledEnum"
        label="状态"
        align="center"
        width="80px"
      >
        <template v-slot:header>
          <el-tooltip class="item" effect="light" :content="`工厂被禁用后，该工厂无法再在其他页面中显示`" placement="top">
            <div style="display: inline-block">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.boolEnabledEnum"
            :disabled="!useCheckPermission(permission.editStatus)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeStatus(scope.row, scope.row.boolEnabledEnum)"
          />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="80px" />
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="160px"
      />
      <!--编辑与删除-->
      <el-table-column
        v-if="useCheckPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </el-card>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/mes/production-config/factory'
import { ref, defineEmits } from 'vue'
import { ElMessageBox } from 'element-plus'

import { enabledEnum } from '@enum-ms/common'
import { TAG_FACTORY_DEF_COLOR } from '@/settings/config'
import useCheckPermission from '@compos/use-check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const emit = defineEmits(['click-factory'])

// crud交由presenter持有
const permission = {
  get: ['factory:get'],
  add: ['factory:add'],
  edit: ['factory:edit'],
  del: ['factory:del'],
  editStatus: ['factory:editStatus']
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
    title: '工厂',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['shortName']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, boolEnabledEnum: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更工厂状态', error)
    data.boolEnabledEnum = data.boolEnabledEnum === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

function handleCurrentChange(val) {
  if (val) {
    emit('click-factory', val)
  }
}
</script>

<style lang="scss" scoped>
.color-card {
  display: inline-block;
  width: 50px;
  height: 20px;
  vertical-align: middle;
}
</style>
