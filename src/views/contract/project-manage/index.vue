<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType" @projectChange="handleChange"/>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left"/>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="false"
        min-width="130"
        align="center"
        label="合同编号"
        fixed="left"
      />
      <el-table-column v-if="columns.visible('shortName')" key="shortName" prop="shortName" :show-overflow-tooltip="true" label="项目" min-width="150">
        <template v-slot="scope">
          <el-tooltip :content="scope.row.serialNumber+' '+scope.row.name" :show-after="50" placement="top">
            <span>{{scope.row.shortName}}</span>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('businessType')"
        key="businessType"
        prop="businessType"
        label="业务类型"
        align="center"
        width="90"
      >
        <template v-slot="scope">
          <div>{{ scope.row.businessType ? businessTypeEnum.VL[scope.row.businessType] : '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectType')"
        key="projectType"
        prop="projectType"
        label="项目类型"
        align="center"
        width="100"
      >
        <template v-slot="scope">
          <div>{{ scope.row.projectType ? projectTypeEnum.VL[scope.row.projectType] : '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectContent')"
        key="projectContent"
        :show-overflow-tooltip="true"
        prop="projectContent"
        label="项目内容"
        align="center"
        width="160"
      >
        <template v-slot="scope">
          <div v-if="scope.row.projectContent" style="width:150px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;">{{ scope.row.projectContent }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('contractAmount')"
        key="contractAmount"
        prop="contractAmount"
        :show-overflow-tooltip="true"
        min-width="120"
        align="center"
        :label="`合同金额(元)`"
      >
        <template v-slot="scope">
          <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('allDays')" key="allDays" prop="allDays" label="总工期(天)" align="center" width="80">
        <template v-slot="scope">
          <div>{{ scope.row.allDays }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('alreadyDays')"
        key="alreadyDays"
        prop="alreadyDays"
        label="已用时(天)"
        align="center"
        width="80"
      >
        <template v-slot="scope">
          <div>{{ scope.row.alreadyDays }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('signerName')" key="signerName" prop="signerName" label="签约人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.signerName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('signingDate')"
        key="signingDate"
        prop="signingDate"
        :show-overflow-tooltip="true"
        align="center"
        width="90"
        label="签订日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.signingDate? parseTime(scope.row.signingDate,'{y}-{m}-{d}'): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        label="立项日期"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" width="120" align="center" fixed="right">
        <template v-slot="scope">
          <el-select v-if="checkPermission(permission.editStatus) && scope.row.status!==projectStatusEnum.SETTLED.V" v-model="scope.row.status" size="small" @change="changeStatus(scope.row, scope.row.status)">
            <template v-for="item in projectStatusEnum.ENUM">
              <el-option
                :key="item.V"
                :label="item.L"
                :value="item.V"
                v-if="item.V!= projectStatusEnum.SETTLED.V"
              />
            </template>
          </el-select>
          <el-tag v-else :type="scope.row.statusColor">{{ projectStatusEnum.VL[scope.row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="成员管理" width="80" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            v-if="checkPermission(permission.editMembers)"
            type="info"
            icon="el-icon-user"
            size="mini"
            @click="showMemberList(scope.row.id)"
            :disabled="scope.row.status !== projectStatusEnum.ENUM.PROCESS.V"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-if="checkPermission([...permission.detail, ...permission.del,, ...permission.download])" label="操作" width="180px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            v-if="checkPermission(permission.detail)"
            size="mini"
            icon="el-icon-view"
            type="primary"
            @click="openContractInfo(scope.row)"
          />
          <udOperation :data="scope.row" :show-edit="false" :permission="permission"/>
          <!-- 下载 -->
           <export-button :fn="downloadProjectInfo" :params="{ projectId: scope.row.id }" v-permission="permission.download"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm />
    <contract-info
      :project-id="contractInfoRowId"
      :project-status="projectStatus"
      :project-name="projectName"
      v-model="contractInfoVisible"
    />
    <common-dialog
      title="项目成员"
      v-model="membersDialogVisible"
      :before-close="
        () => {
          membersDialogVisible = false
        }
      "
      :show-close="true"
      :close-on-click-modal="false"
      top="8vh"
      width="500px"
    >
      <members
        ref="membersList"
        :project-id="currentProjectId"
        :permission="permission"
        :refresh="membersDialogVisible"
        @success="memberChangeSuccess"
      />
    </common-dialog>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi, { editStatus, downloadProjectInfo } from '@/api/contract/project'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { projectTypeEnum, businessTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { ElMessageBox } from 'element-plus'
import contractInfo from '@/views/contract/info/index'
import members from './members'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { useStore } from 'vuex'
import { projectListPM as permission } from '@/page-permission/contract'
import { ElSelect } from 'element-plus'
import ExportButton from '@comp-common/export-button/index.vue'

const store = useStore()
const { currentProjectType } = mapGetters(['globalProjectId', 'currentProjectType'])

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentProjectId = ref()
const membersDialogVisible = ref(false)
const contractInfoRowId = ref()
const projectStatus = ref()
const projectName = ref()
const contractInfoVisible = ref(false)
const membersList = ref()
// const infoDialog = ref()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '合同档案',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractProject',
  paginate: true,
  extraHeight: 40
})

function showMemberList(id) {
  currentProjectId.value = id
  membersDialogVisible.value = true
}

function memberChangeSuccess() {
  membersDialogVisible.value = false
  crud.toQuery()
}

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm(`此操作将把 “${data.name}” 状态更改为 “${projectStatusEnum.VL[val]}”, 是否继续？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id, val, false)
    crud.notify(`“${data.name}” 变更为 “${projectStatusEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
    handleChange()
  } catch (error) {
    console.log(error)
    data.status = data.orginStauts
  }
}

function openContractInfo(row) {
  contractInfoRowId.value = row.id
  projectStatus.value = row.status
  projectName.value = row.name
  contractInfoVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.statusColor = v.status === projectStatusEnum.COMPLETE.V ? 'success' : v.status === projectStatusEnum.SUSPEND.V ? 'warning' : null
    v.orginStauts = v.status
    return v
  })
}

function handleChange() {
  store.dispatch('project/fetchUserProjects')
  store.dispatch('project/fetchProjectTree')
}
CRUD.HOOK.afterDelete = () => {
  handleChange()
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
