<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType"/>
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
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="false" min-width="130" label="合同编号" />
    <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" min-width="150" label="项目名称" />
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="业务类型" align="center" width="100">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="项目类型" align="center" width="100">
      <template v-slot="scope">
        <div>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" :show-overflow-tooltip="true" prop="projectContent" label="项目内容" align="center" min-width="150">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" min-width="120" align="center" :label="`合同金额(元)`">
      <template v-slot="scope">
        <!-- <span>{{ scope.row.contractAmount? scope.row.contractAmount.toFixed(DP.YUAN): '-' }}</span> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('allDays')" key="allDays" prop="allDays" label="总工期(天)" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.allDays }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('alreadyDays')" key="alreadyDays" prop="alreadyDays" label="已用时(天)" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.alreadyDays }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('signerName')" key="signerName" prop="signerName" label="签约人" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.signerName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('signingDate')" key="signingDate" prop="signingDate" :show-overflow-tooltip="true" width="100" label="签订日期">
      <template v-slot="scope">
        <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.signingDate }}</span>
      </template>
    </el-table-column>

    <el-table-column v-if="columns.visible('createDate')" key="createDate" prop="createDate" label="立项日期" align="center" width="100px">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createDate }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" width="110px" align="center" fixed="right">
      <template v-slot="scope">
        <span>{{ isNotBlank(scope.row.status)? projectStatusEnum.VL[scope.row.status]:'-' }}</span>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="checkPermission(permission.editMembers)" label="成员管理" width="90px" align="center" fixed="right">
      <template v-slot="scope">
        <common-button v-permission="permission.editMembers" type="info" icon="el-icon-user" size="mini" @click="showMemberList(scope.row.id)" />
      </template>
    </el-table-column> -->
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.download])"
      label="操作"
      width="260px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button v-if="checkPermission(permission.detail)" size="mini" icon="el-icon-view" type="primary" @click="openContractInfo(scope.row)" />
        <common-button v-if="checkPermission(permission.editStatus) && scope.row.status===projectStatusEnum.ENUM.PROCESS.V" size="mini" @click="changeStatus(scope.row,projectStatusEnum.ENUM.SUSPEND.V)">暂停</common-button>
        <common-button v-if="checkPermission(permission.editStatus) && scope.row.status===projectStatusEnum.ENUM.SUSPEND.V" size="mini" @click="changeStatus(scope.row,projectStatusEnum.PROCESS.v)">继续</common-button>
        <udOperation
          :data="scope.row"
          :show-edit="false"
        />
        <!-- 下载 -->
        <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
      </template>
    </el-table-column>
  </common-table>
  <mForm />
  <common-drawer
    v-model="contractInfoVisible"
    :with-header="false"
    direction="rtl"
    size="80%"
    :before-close="() => {contractInfoVisible = false}"
  >
    <template #content>
      <contract-info :project-id="contractInfoRowId" :project-staus="projectStaus" :project-name="projectName" style="padding:20px;box-sizing:border-box" />
    </template> 
  </common-drawer>
  <!-- <common-dialog
    class="members-dialog"
    title="项目成员"
    v-model="membersDialogVisible"
    top="8vh"
    width="500px"
    :center="false"
  >
      <members
        ref="membersList"
        :project-id="currentProjectId"
        :permission="permission"
        :refresh="membersDialogVisible"
      />
      <template #footer>
        <span class="dialog-footer">
          <common-button type="primary" @click="membersDialogVisible = false">退 出</common-button>
        </span>
      </template>  
    </common-dialog> -->
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/contract/project'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { projectTypeEnumN, businessTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { isNotBlank } from '@data-type/index'
import { ElMessageBox } from 'element-plus'
import contractInfo from '@/views/contract/info/index'
// import members from './members'

const { globalProjectId, currentProjectType } = mapGetters(['globalProjectId', 'currentProjectType'])
// crud交由presenter持有
const permission = {
  get: ['contract:get'],
  add: ['contract:add'],
  detail: ['contract:detail'],
  editStatus: ['contract:editStatus'],
  download: ['contract:download']
}

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const currentProjectId = ref()
const membersDialogVisible = ref(false)
const contractInfoRowId = ref()
const projectStaus = ref()
const projectName = ref()
const contractInfoVisible = ref(false)

const { crud, columns, CRUD } = useCRUD(
  {
    title: '合同档案',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  }
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractProject',
  paginate: true,
  extraHeight: 157
})

function showMemberList(id) {
  currentProjectId.value = id
  membersDialogVisible.value = true
}

async function changeStatus(data, val) {
  try {
    const msg = val === projectStatusEnum.ENUM.SUSPEND.V ? '继续' : '暂停'
    await ElMessageBox.confirm(`确定${msg}“${data.name}” 项目吗 ？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id, val)
    crud.notify(`“${data.name}” 变更为 “${projectStatusEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
  } catch (error) {
    console.log(error)
    data.status = data.status === projectStatusEnum.ENUM.SUSPEND.V? projectStatusEnum.ENUM.PROCESS.V: projectStatusEnum.ENUM.SUSPEND.V
  }
}

function openContractInfo(row){
  contractInfoRowId.value = row.id
  projectStaus.value = row.status
  projectName.value = row.name
  contractInfoVisible.value = true
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