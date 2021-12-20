<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!-- 表格渲染 -->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight()"
      style="width: 100%;"
      @selection-change="crud.selectionChangeHandler"
      @row-dblclick="changeDefault"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="70" />
      <el-table-column v-if="columns.visible('templateName')" key="templateName" prop="name" :show-overflow-tooltip="true" label="模板名称" min-width="130px" align="left">
        <template v-slot="scope">
          <table-cell-tag v-show="scope.row.isDefault" name="默认" />
          <span>{{ scope.row.templateName | emptyTextFormatter }}
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="描述信息" min-width="100px" align="left">
        <template v-slot="scope"><span>{{ scope.row.remark | emptyTextFormatter }}</span></template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" :show-overflow-tooltip="true" label="状态" min-width="100px" align="center">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.status"
            :disabled="!checkPermission(permission.edit)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeEnabled(scope.row, scope.row.status)"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="170"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation
            :data="scope.row"
          />
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script>
// import tableCellTag from '@/views/components/common/table-cell-tag'
// import crudMethod, { editStatus, editDefault } from '@/api/config-manage/project-nember-template'
// import CRUD, { presenter } from '@crud/crud'
// import mHeader from './module/header'
// import enumOperate, { enabledEnum } from '@/utils/enum/index'
// import udOperation from '@crud/UD.operation'
// import mForm from './module/form'
// import checkPermission from '@/utils/permission'
// import pagination from '@crud/Pagination'

// const enabledEnumV = enumOperate.getVal(enabledEnum)

// // crud交由presenter持有
// const permission = {
//   get: ['ProjectMemberTemplate:get'],
//   get: ['ProjectMemberTemplateDetail:get'],
//   add: ['ProjectMemberTemplate:add'],
//   edit: ['ProjectMemberTemplate:edit'],
//   del: ['ProjectMemberTemplate:del'],
//   post: ['ProjectMemberTemplateStatus:post'],
//   put: ['ModifyDefaultTemplate:put']
// }

// const optShow = {
//   add: true,
//   edit: false,
//   del: false,
//   download: false
// }

// const crud = CRUD({
//   title: '项目成员模板',
//   sort: ['sort.asc', 'id.desc'],
//   permission: { ...permission },
//   optShow: { ...optShow },
//   hasPagination: false,
//   crudMethod: { ...crudMethod }
// })

// export default {
//   name: 'ProjectMemberTemplate',
//   components: { pagination, mHeader, udOperation, mForm, tableCellTag },
//   mixins: [presenter(crud)],
//   inject: ['$_tableMaxHeight'],
//   data() {
//     return {
//       permission,
//       enabledEnum,
//       membersDialogVisible: false,
//       currentProjectId: undefined,
//       enabledEnumV
//     }
//   },

//   methods: {
//     checkPermission,
//     async changeEnabled(data, val) {
//       try {
//         await this.$confirm('此操作将 "' + enabledEnumV[val].L + '" ' + data.templateName + ', 是否继续？', '提示', {
//           confirmButtonText: '确定',
//           cancelButtonText: '取消',
//           type: 'warning'
//         })
//         await editStatus({ id: data.id, status: val })
//         this.crud.refresh()
//         this.crud.notify(enabledEnumV[val].L + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
//       } catch (error) {
//         console.log('操作状态', error)
//         data.status = data.status === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
//       }
//     },
//     async changeDefault(row) {
//       try {
//         const isDefault = !row.isDefault
//         const tip = isDefault ? `此操作将“${row.templateName}”设置为该表格的默认模板` : `此操作取消“${row.templateName}”的默认状态`
//         await this.$confirm(tip, '提示', {
//           confirmButtonText: '确定',
//           cancelButtonText: '取消',
//           type: 'warning'
//         })
//         await editDefault({ id: row.id, isDefault })
//         this.crud.refresh()
//         this.crud.notify(isDefault ? '设置' : '取消' + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
//       } catch (error) {
//         console.log('变更表格状态', error)
//       }
//     }
//   }
// }
</script>

