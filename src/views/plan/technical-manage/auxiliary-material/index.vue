<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader :project-id="globalProjectId" />
    <!--表格渲染-->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight()"
      style="width: 100%;"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" min-width="60" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" sortable="custom" :show-overflow-tooltip="true" label="编号" min-width="120" />
      <el-table-column v-if="columns.visible('fullClassName')" key="fullClassName" prop="fullClassName" :show-overflow-tooltip="true" label="辅材类别" min-width="250" />
      <el-table-column v-if="columns.visible('unitValue')" key="unitValue" prop="unitValue" sortable="custom" label="单位" min-width="80px">
        <template v-slot="scope">
          {{ scope.row.unit }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('color')" key="color" prop="color" sortable="custom" :show-overflow-tooltip="true" label="颜色" min-width="120" />
      <el-table-column v-if="columns.visible('specification')" key="specification" prop="specification" sortable="custom" :show-overflow-tooltip="true" label="规格" min-width="120" />
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" sortable="custom" label="数量" align="left" min-width="80px" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="120" />
      <el-table-column v-if="columns.visible('createUser')" key="createUser" prop="createUser" :show-overflow-tooltip="true" label="上传人" min-width="110" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="上传时间" min-width="160px">
        <template v-slot="scope">
          <div>{{ scope.row.createTime | parseTime }}</div>
        </template>
      </el-table-column>
      <!--状态、编辑与删除-->
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="80px" fixed="right">
        <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`构件进行与暂停: \n
          1.无论有无生产均可以执行暂停；\n
          2.暂停后，无法扫码上传。\n`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.status"
            :disabled="!checkPermission(permission.edit)"
            active-color="#13ce66"
            :active-value="processingEnum.PROCESS.V"
            :inactive-value="processingEnum.PAUSE.V"
            @change="changeStatus(scope.row, scope.row.status)"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.changeQuantity])"
        label="操作"
        width="115px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation
            :data="scope.row"
            :show-edit="false"
          />
          <!--          <del-form-->
          <!--            :data="scope.row"-->
          <!--            :permission="permission"-->
          <!--            :project-id="globalProjectId"-->
          <!--            @success="handleNumberChange"-->
          <!--          />-->
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import crudMethod, { editStatus } from '@/api/mes-plan/technical-manage/auxiliary-material'
import CRUD, { presenter } from '@crud/crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import reasonDescription from '@/mixins/reason-description'
import mHeader from './module/header'
import mForm from './module/form'
// import delForm from './module/del'
import enumOperate, { processingEnum } from '@/utils/enum/index'
import checkPermission from '@/utils/system/check-permission'
// crud交由presenter持有
const permission = {
  get: ['auxiliaryMaterial:get'],
  add: ['auxiliaryMaterial:add'],
  edit: ['auxiliaryMaterial:edit'],
  changeQuantity: ['auxiliaryMaterial:changeQuantity'],
  clear: ['auxiliaryMaterial:clearWithOneClick']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const crud = CRUD({
  title: '配套件清单',
  permission: { ...permission },
  crudMethod: { ...crudMethod },
  optShow: { ...optShow },
  requiredQuery: ['areaId'],
  invisibleColumns: ['createUser', 'createTime'],
  queryOnPresenterCreated: false
})

const processingEnumV = enumOperate.getVal(processingEnum)

export default {
  name: 'MesAuxiliaryMaterialList',
  components: { mHeader, mForm, pagination, udOperation },
  mixins: [presenter(crud), reasonDescription],
  inject: ['$_tableMaxHeight'],
  data() {
    return {
      permission,
      processingEnum,
      processingEnumV
    }
  },
  computed: {
    ...mapGetters([
      'globalProjectId'
    ])
  },
  methods: {
    checkPermission,
    handleNumberChange() {
      this.$notify({ title: '变更成功', type: 'success', duration: 2500 })
      this.crud.toQuery()
    },
    async changeStatus(data, val) {
      try {
        const editData = {
          id: data.id,
          status: val
        }
        // 暂停时需要填写原因
        if (val === processingEnum.PAUSE.V) {
          const remark = await this.$openReasonMsg({
            tip: `此操作将 【${processingEnumV[val].L}】“${data.thirdName} ${data.serialNumber}”`,
            title: '请描述原因'
          })
          editData.remark = remark
        }
        await editStatus(editData)
        this.$notify({ title: `“${data.thirdName} ${data.serialNumber}”已【${processingEnumV[val].L}】`, type: 'success', duration: 2500 })
      } catch (error) {
        console.log('修改辅材状态', error)
        data.status = data.status === processingEnum.PROCESS.V ? processingEnum.PAUSE.V : processingEnum.PROCESS.V
      }
    }

  }
}
</script>
