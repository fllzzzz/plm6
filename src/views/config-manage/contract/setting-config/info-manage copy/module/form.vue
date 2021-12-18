<template>
  <el-drawer
    :title="crud.status.title"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    direction="rtl"
    size="40%"
    render-after-expand="false"
  >
    <template slot="title">
      <span>{{ crud.status.title }}</span>
    </template>
    <div :max-height="$_height" class="dialog-container">
      <el-form ref="form" :model="form" :rules="rules" size="small" label-width="90px">
        <el-form-item label="模板名称" prop="templateName">
          <el-input
            v-model="form.templateName"
            placeholder="模板名称"
            maxlength="20"
            size="small"
            clearable
            class="input-underline"
            style="width: 350px;"
          />
        </el-form-item>
        <el-form-item label="描述信息" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 4}"
            placeholder="描述信息"
            style="max-width: 350px;"
          />
        </el-form-item>
        <el-form-item label="默认模板" prop="isDefault">
          <el-checkbox v-model="form.isDefault" />
        </el-form-item>
        <el-form-item label="用户信息" prop="userIdList">
          <div class="filter-left-box">
            <el-input
              v-model="filterText"
              class="filter-item"
              placeholder="输入关键字进行过滤"
              style="width:270px"
            />
          </div>
          <div
            class="user-tree tree-container"
          >
            <el-tree
              ref="tree"
              class="filter-tree"
              :data="userList"
              :empty-text="'没有用户'"
              :props="defaultProps"
              default-expand-all
              show-checkbox
              node-key="id"
              :filter-node-method="filterNode"
              :default-checked-keys="checkedList"
            />
          </div>
        </el-form-item>
      </el-form>
    </div>
    <div class="demo-drawer__footer">
      <el-button type="text" @click="crud.cancelCU">取消</el-button>
      <el-button :loading="crud.status.cu === 2" size="small" type="primary" @click="crud.submitCU">确认</el-button>
    </div>
  </el-drawer>
</template>

<script>
import CRUD, { form } from '@crud/crud'
import sizeCalc from '@/mixins/sizeCalc'
import { getUserTree } from '@/api/common'
import { enabledEnum } from '@/utils/enum/index'
import { getUserAllSimpleByProject as getAllUser } from '@/api/config-manage/project-nember-template'

const defaultForm = {
  id: void 0,
  templateName: void 0,
  status: enabledEnum.TRUE.V,
  remark: void 0,
  isDefault: false,
  userIdList: []
}
export default {
  mixins: [form(defaultForm), sizeCalc],
  data() {
    return {
      filterText: '',
      extraHeight: 1,
      checkedList: [],
      disabledUser: [],
      noDisabledUser: [],
      enabledEnum,
      defaultProps: {
        children: 'children',
        label: 'label'
      },
      rules: {
        templateName: [
          { required: true, message: '请填写模板名称', trigger: 'change', type: 'string' }
        ],
        status: [
          { required: true, message: '请选择状态', trigger: 'blur', type: 'number' }
        ],
        remark: [
          { required: true, message: '请填写描述信息', trigger: 'change', type: 'string' }
        ]
      }
    }
  },
  computed: {
    userList() {
      return this.isEditing ? this.noDisabledUser : this.disabledUser
    }
  },
  watch: {
    filterText(val) {
      this.$refs.tree.filter(val)
    },
    refresh(val) {
      if (val) {
        this.fetchMembers()
      } else {
        this.checkedList = []
        this.resetChecked()
      }
    }
  },
  created() {
    this.fetchUserTree()
  },
  methods: {
    filterNode(value, data) {
      if (!value) return true
      return data.label.indexOf(value) !== -1 || (data.parentLabel && data.parentLabel.indexOf(value) !== -1)
    },
    traversalTree(tree, disabled, parentLabel) {
      tree.forEach(node => {
        node.disabled = disabled
        node.parentLabel = parentLabel
        if (!node.isUser) {
          if (node.id === 1) {
            node.label = '<公司>' + node.label
          } else {
            node.label = '<部门>' + node.label
          }
          node.id = -node.id
        } else {
          // TODO: 同名
        }
        // TODO: 可能需要对为空的部门做处理
        if (node.children) {
          this.traversalTree(node.children, disabled, node.label)
        }
      })
    },
    async fetchUserTree() {
      let disabledUser = []
      let noDisabledUser = []
      try {
        const data = await getUserTree()
        const user = []
        user.push(data)
        const tree = user
        disabledUser = JSON.parse(JSON.stringify(tree))
        this.traversalTree(disabledUser)
        noDisabledUser = JSON.parse(JSON.stringify(tree))
        this.traversalTree(noDisabledUser)
      } catch (error) {
        console.log(error)
      } finally {
        this.disabledUser = disabledUser
        this.noDisabledUser = noDisabledUser
      }
    },
    async fetchMembers() {
      let userIds = []
      try {
        const { content } = await getAllUser({ id: this.form.id })
        userIds = content.map(v => v.id)
      } catch (error) {
        console.log(error)
      } finally {
        this.checkedList = userIds
        this.resetChecked()
      }
    },
    resetChecked() {
      this.$refs.tree.setCheckedKeys(this.userList)
    },
    [CRUD.HOOK.afterToEdit]() {
      this.fetchMembers()
    },
    [CRUD.HOOK.beforeSubmit]() {
      let checkedNodes = this.$refs.tree.getCheckedKeys()
      checkedNodes = checkedNodes.filter(item => item > 0)
      this.checkedList = checkedNodes
      this.form.userIdList = checkedNodes
    },
    [CRUD.HOOK.beforeToAdd]() {
      this.$nextTick(() => {
        if (this.$refs.tree) {
          this.resetChecked()
        }
        this.filterText = ''
      })
    }
  }
}
</script>

<style  lang="scss" scoped>
.demo-drawer__footer{
  z-index: 999999;
}
.tree-container {
    height: 55vh;
    overflow-y: auto;
    .filter-tree {
        height:295px;
    }
}
.user-tree{
  margin-top: 5px;
  height: 500px;
}
</style>
